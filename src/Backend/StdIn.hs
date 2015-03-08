module Backend.StdIn where

import Control.Auto
import Control.Auto.Run
import Control.Exception
import Control.Monad     (void)
import Data.Foldable     (mapM_)
import Data.Maybe
import Data.Time
import Prelude hiding    ((.), id, mapM_)
import System.IO
import Types

stdinLoop :: ChatBot IO -> IO ()
stdinLoop = void . run getFirstInp processOutp
  where
    getFirstInp :: IO (Either ChronEvent InMessage)
    getFirstInp = fromJust <$> getInp
    getInp :: IO (Maybe (Either ChronEvent InMessage))
    getInp = do
      putStr "> "
      hFlush stdout
      inp <- try getLine :: IO (Either SomeException String)
      case inp of
        Right inp' -> do
          t <- getCurrentTime
          return . Just . Right $ InMessage "justin" inp' "#stdin" t
        Left _ -> return Nothing
    processOutp :: OutMessages -> IO (Maybe (Either ChronEvent InMessage))
    processOutp (OutMessages out) = do
      mapM_ (mapM_ putStrLn) out
      getInp
