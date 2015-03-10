{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Module.Github where

-- import Network.HTTP.Client.TLS
-- import qualified Text.JSON         as J
import Auto.Chatbot
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Serialize
import Control.Auto.Time
import Control.Exception
import Control.Lens
import Control.Monad                  (join)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Data.Foldable                  (foldMap)
import Data.Maybe
import Data.Text.Lens
import Data.Time
import Data.Traversable               (forM)
import Network.Wreq
import Prelude hiding                 ((.), id)
import System.Locale
import qualified Data.Map             as M
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

maxUpdates :: Int
maxUpdates = 6

updateDelay :: Integer      -- in seconds
updateDelay = 10 * 60

messageLimit :: Int
messageLimit = 250

repos :: [(Repo, [Channel])]
repos = [ (Repo "mstksg" "auto"         , ["#haskell-auto"])
        , (Repo "mstksg" "auto-examples", ["#haskell-auto"])
        , (Repo "mstksg" "auto-chatbot" , ["#haskell-auto"])
        , (Repo "mstksg" "jlebot2"      , ["#haskell-auto", "#ucsd"])
        ]

githubBot :: (MonadIO m, MonadFix m) => FilePath -> ChatBotChron m
githubBot srlzFp = foldMap (uncurry (githubBotRepo srlzFp)) repos

githubBotRepo :: (MonadFix m, MonadIO m)
              => FilePath -> Repo -> [Channel] -> ChatBotChron m
githubBotRepo srlzFp repo dests = serializing' srlzFp' $ proc time -> do
    rec lastPushId <- lastVal 0 . holdWith 0 . accumB max 0 -< newLastId
        newSecond <- onChange -< round (utctDayTime time) `div` updateDelay
        newPushes <- perBlip (arrM (liftIO . getPushes repo)) -< lastPushId <$ newSecond
        pushBlip <- filterB (not . null) -< newPushes
        let newLastId = maximum . map pushId <$> pushBlip
    let msgsBlip   = rateLimit . mkMessages <$> pushBlip
        msgMapBlip = OutMessages . M.fromList
                   . zip dests . repeat
                 <$> msgsBlip
    fromBlips mempty -< msgMapBlip
  where
    rateLimit xs | length xs <= maxUpdates = xs
                 | otherwise = take maxUpdates xs ++ ["... and more ..."]
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
                     ) : map commitLine (pushCommits p)
          where
            commitLine c = "\x02" ++ pushRepo p ++ "\x0f"
                        ++ "/" ++ pushBranch p
                        ++ take 7 (commitSHA c)
                        ++ " \x02" ++ commitAuthor c ++ "\x0f: "
                        ++ (take messageLimit . concat . take 1 . lines $ commitMessage c)
                        ++ " (\x1f" ++ commitUrl c ++ "\x0f)"


getPushes :: Repo -> Integer -> IO [Push]
getPushes (Repo rowner rname) limit = do
    putStrLn $ "Fetching repo " ++ rowner ++ "/" ++ rname
    let reqUrl = "https://api.github.com/repos/"
               <> rowner <> "/"
               <> rname <> "/events"
    handle handler . S.withSession $ \sess -> do
      r <- S.get sess reqUrl
      print r
      let rawPushes = r ^.. responseBody . values
      pushes <- rawPushes & each %%~ \rawPush -> do
        let pId    = rawPush ^? key "payload" . key "push_id" . _Integer
            pRepo  = rawPush ^? key "repo" . key "name" . _String . re packed . to getFinal
            pBrnch = rawPush ^? key "payload" . key "ref" . _String . re packed . to getFinal
            pActor = rawPush ^? key "actor" . key "login" . _String . re packed
            pTime  = rawPush ^? key "created_at" . _String . re packed . _iso8601
            pComs  = rawPush ^.. key "payload" . key "commits" . values

        print (pId, pRepo, pBrnch, pActor, pTime)

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
          else do
            print ("No new events found!" :: String)
            return Nothing

      return $ reverse (catMaybes pushes)
  where
    getFinal = reverse . takeWhile (/= '/') . reverse
    _utf8 = prism T.encodeUtf8 (left mempty . T.decodeUtf8')
    handler :: C.HttpException -> IO [Push]
    handler e = [] <$ print e

_iso8601 :: (Show a, ParseTime a) => Prism' String a
_iso8601 = prism' show (parseTime defaultTimeLocale tFormat)
  where
    tFormat = iso8601DateFormat (Just "%H:%M:%SZ")




-- getPushes :: Repo -> IO (Maybe [Push])
-- getPushes (Repo rowner rname) = do
--     let purl = parseUrl . concat $ ["https://api.github.com/repos/"
--                                    , rowner , "/" , rname , "/events"
--                                    ]
--     case purl of
--       Nothing  -> return Nothing
--       Just req ->
--         withManager tlsManagerSettings $ \manager -> do
--                               -- errorable!
--           resp <- T.unpack . TdecodUtf8 . reponseBody <$> httpLbs req manager
--           let parsedResp = resultToEither . decode $ resp
--           case parsedResp of
--             Left _ -> return Nothing
--             Right rawDat ->
--               case rawDat of
--                 J.JSArray rawPushes ->
--                   fmap (Just . catMaybes) . forM_ rawPushes $ \rawPush -> do
--                     case rawPush of
--                       J.JSObject pushObj -> do
--                         let pId = get_field pushObj "id"
--                             pUsr = undefined
--                         undefined

--                       _ -> return Nothing
--                 _                 -> return Nothing





-- getLast :: String -> IO String
-- getWithin url = do
--     let purl = parseUrl url
--     case purl of
--       Nothing -> error "ah"
--       Just req ->
--         withManager tlsManagerSettings $ \manager -> do
--           resp <- T.unpack . decodeUtf8 . responseBody <$> httpLbs req manager
--           print resp
--           let parsedM = parseFeedString resp
--           case parsedM of
--             Just (AtomFeed feed) -> do
--               let entries = feedEntries feed
--                   firstEntry = head entries
--               return $ unlines [ entryId firstEntry
--                                , show (entryTitle firstEntry)
--                                , show . parseEDate . entryUpdated $ firstEntry
--                                , show (entryContent firstEntry)
--                                , show (entryAuthors firstEntry)
--                                ]
--             _ -> return (show parsedM)


-- testFeed = "https://github.com/mstksg/auto/commits/develop.atom"
