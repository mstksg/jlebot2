module Module.RSS where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.Time
import Data.Time.Format
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Locale
import Text.Atom.Feed
import Text.Feed.Import
import Text.Feed.Types

testFeed = "https://github.com/mstksg/auto/commits/develop.atom"

getLatest :: String -> IO String
getLatest url = do
    let purl = parseUrl url
    case purl of
      Nothing -> error "ah"
      Just req ->
        withManager tlsManagerSettings $ \manager -> do
          resp <- T.unpack . decodeUtf8 . responseBody <$> httpLbs req manager
          print resp
          let parsedM = parseFeedString resp
          case parsedM of
            Just (AtomFeed feed) -> do
              let entries = feedEntries feed
                  firstEntry = head entries
              return $ unlines [ entryId firstEntry
                               , show (entryTitle firstEntry)
                               , show . parseEDate . entryUpdated $ firstEntry
                               , show (entryContent firstEntry)
                               , show (entryAuthors firstEntry)
                               ]
            _ -> return (show parsedM)

parseEDate :: String -> Maybe UTCTime
parseEDate = parseTime defaultTimeLocale eDateFormat

eDateFormat :: String
eDateFormat = iso8601DateFormat (Just "%H:%M:%S%z")

-- "2015-03-08T03:45:27-07:00"

rssBot = undefined
