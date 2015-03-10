{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Module.Github where

import Auto.Chatbot
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Serialize
import Control.Exception
import Control.Lens
import Control.Monad                  (join)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Data.ByteString                (ByteString)
import Data.Foldable                  (foldMap)
import Data.Maybe
import Data.Text.Lens
import Data.Time
import Data.Traversable               (forM)
import Instances                      ()
import Network.Wreq
import Prelude hiding                 ((.), id)
import System.Locale
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Client  as C
import qualified Network.Wreq.Session as S

data Repo = Repo { repoOwner  :: String
                 , repoName   :: String
                 } deriving Show

data Commit = Commit { commitSHA     :: String
                     , commitAuthor  :: String
                     , commitMessage :: String
                     , commitUrl     :: String
                     } deriving Show

data Push = Push { pushId      :: Integer
                 , pushRepo    :: String
                 , pushBranch  :: String
                 , pushActor   :: String
                 , pushTime    :: UTCTime
                 , pushCommits :: [Commit]
                 } deriving Show

data Fork = Fork { forkId    :: Integer
                 , forkName  :: String
                 , forkOwner :: String
                 , forkUrl   :: String
                 } deriving Show

data Event = EventPush Push
           | EventFork Fork
           deriving Show

type ETag = ByteString

log' :: String -> IO ()
log' = putStrLn . ("[GITHUB]: " ++)

maxUpdates :: Int
maxUpdates = 3

maxCommits :: Int
maxCommits = 3

updateDelay :: Integer      -- in seconds
updateDelay = 2 * 60

messageLimit :: Int
messageLimit = 250

repos :: [(Repo, [Channel])]
repos = [ (Repo "mstksg" "auto"         , ["#haskell-auto"])
        , (Repo "mstksg" "auto-examples", ["#haskell-auto"])
        , (Repo "mstksg" "auto-chatbot" , ["#haskell-auto"])
        , (Repo "mstksg" "jlebot2"      , ["#haskell-auto"])
        ]

githubBot :: (MonadIO m, MonadFix m) => FilePath -> ChatBotChron m
githubBot srlzFp = foldMap (uncurry (githubBotRepo srlzFp)) repos

githubBotRepo :: (MonadFix m, MonadIO m)
              => FilePath -> Repo -> [Channel] -> ChatBotChron m
githubBotRepo srlzFp repo dests = serializing' srlzFp' $ proc time -> do
    rec lastPushId <- holdWith 0 . accumB max 0 . lagBlips -< lastIdBlip
        lastETag   <- holdWith ""               . lagBlips -< lastETagBlip

        readyBlip <- onChange -< round (utctDayTime time) `div` updateDelay

        let newRequest = (lastPushId, lastETag) <$ readyBlip
        newPushes <- perBlip (arrM (liftIO . getPushes repo)) -< (lastPushId, lastETag) <$ newSecond
        pushBlip <- filterB (not . null . fst) -< newPushes

        let lastIdBlip = maximum . map pushId . fst <$> pushBlip
            lastETagBlip = snd <$> newPushes

    let msgsBlip   = mkMessages . reverse . take maxUpdates . fst <$> pushBlip
        msgMapBlip = OutMessages . M.fromList
                   . zip dests . repeat
                 <$> msgsBlip

    fromBlips mempty -< msgMapBlip
  where
    srlzFp' = srlzFp ++ "-" ++ repoOwner repo ++ "-" ++ repoName repo
    mkMessages :: [Push] -> [Message]
    mkMessages = concatMap fromPush
      where
        -- fromPush p = ( "[\x0302,12" ++ pushRepo p ++ "\x0f] "
        --             ++ "\x02" ++ pushActor p ++ "\x0f pushed "
        --             ++ "\x0314" ++ show (length (pushCommits p)) ++ "\x0f"
        --             ++ " new commit(s) to "
        --             ++ "\x0303" ++ pushBranch p ++ "\x0f"
        --              ) : map commitLine (pushCommits p)
        --   where
        --     commitLine c = "\x0302,12" ++ pushRepo p ++ "\x0f"
        --                 ++ "/\x0303" ++ pushBranch p ++ "\x0f "
        --                 ++ "\x0307" ++ take 7 (commitSHA c) ++ "\x0f "
        --                 ++ "\x02" ++ commitAuthor c ++ "\x0f: "
        --                 ++ "\x09" ++ (take 100 . concat . take 1 . lines $ commitMessage c) ++ "\x0f"
        --                 ++ " (\x1f\x0312" ++ commitUrl c ++ "\x0f)"
        fromPush p = ( "[\x02" ++ pushRepo p ++ "\x0f] "
                    ++ pushActor p ++ " pushed "
                    ++ "\x1f" ++ show (length (pushCommits p)) ++ "\x0f"
                    ++ " new commit(s) to "
                    ++ pushBranch p
                     ) : (rateLimit . map commitLine . reverse $ pushCommits p)
          where
            rateLimit xs | length xs <= maxCommits = xs
                         | otherwise = take maxCommits xs ++ ["... and more ..."]
            commitLine c = "\x02" ++ pushRepo p ++ "\x0f"
                        ++ "/" ++ pushBranch p ++ " "
                        ++ take 7 (commitSHA c)
                        ++ " \x02" ++ commitAuthor c ++ "\x0f: "
                        ++ (limitMessage . concat . take 1 . lines $ commitMessage c)
                        ++ " (\x1f" ++ commitUrl c ++ "\x0f)"
            limitMessage msg | length msg <= messageLimit = msg
                             | otherwise = take messageLimit msg ++ "..."


getPushes :: Repo -> (Integer, ETag) -> IO ([Push], ETag)
getPushes (Repo rowner rname) (limit, lastETag) = do
    log' $ "Fetching repo " ++ rowner ++ "/" ++ rname
    let reqUrl = "https://api.github.com/repos/"
               <> rowner <> "/"
               <> rname <> "/events"
        opts = defaults & header "If-None-Match" .~ [ lastETag ]

    result <- handle handler . S.withSession $ \sess -> do
      r <- S.getWith opts sess reqUrl
      let rawPushes = r ^.. responseBody . values
          newETag   = r ^. responseHeader "ETag"
      pushes <- rawPushes & each %%~ \rawPush -> do
        let pId    = rawPush ^? key "payload" . key "push_id" . _Integer
            pRepo  = rawPush ^? key "repo" . key "name" . _String . re packed . to getFinal
            pBrnch = rawPush ^? key "payload" . key "ref" . _String . re packed . to getFinal
            pActor = rawPush ^? key "actor" . key "login" . _String . re packed
            pTime  = rawPush ^? key "created_at" . _String . re packed . _iso8601
            pComs  = rawPush ^.. key "payload" . key "commits" . values

        if pId > Just limit
          then do
            comms <- pComs & each %%~ \rawCommit -> do
              let cSha     = rawCommit ^? key "sha" . _String . re packed
                  cAuthor  = rawCommit ^? key "author" . key "name" . _String . re packed
                  cMsg     = rawCommit ^? key "message" . _String . re packed
                  cUrlBase = "https://github.com/"
                          <> rowner <> "/"
                          <> rname <> "/commit/"
                  cUrl     = fmap (cUrlBase <>) cSha

              cUrlShort <- fmap join . forM cUrl $ \cUrl' -> do
                  gitioR <- S.post sess "http://git.io" [ "url" := cUrl' ]
                  return $ gitioR ^? responseHeader "Location" . _utf8 . re packed

              return $ Commit <$> cSha
                              <*> cAuthor
                              <*> cMsg
                              <*> cUrlShort

            return $ Push <$> pId
                          <*> pRepo
                          <*> pBrnch
                          <*> pActor
                          <*> pTime
                          <*> pure (catMaybes comms)
          else
            return Nothing

      return (catMaybes pushes, newETag)

    log' $ "Found " ++ show (length (fst result)) ++ " event(s)"

    return result
  where
    getFinal = reverse . takeWhile (/= '/') . reverse
    _utf8 :: Prism' ByteString T.Text
    _utf8 = prism T.encodeUtf8 (left mempty . T.decodeUtf8')
    handler :: C.HttpException -> IO ([Push], ETag)
    handler e = case e of
                  C.StatusCodeException s _ _
                      | s ^. statusCode == 304 -> return ([], lastETag)
                  _ -> ([],"") <$ print e

_iso8601 :: (Show a, ParseTime a) => Prism' String a
_iso8601 = prism' show (parseTime defaultTimeLocale tFormat)
  where
    tFormat = iso8601DateFormat (Just "%H:%M:%SZ")

