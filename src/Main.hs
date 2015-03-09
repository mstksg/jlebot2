module Main where

import Auto.Chatbot
import Auto.Chatbot.Backend.IRC
import Auto.Chatbot.Backend.Stdin
import Control.Applicative
import Control.Auto.Serialize
import Data.Monoid
import Module.Greet
import Module.Karma
import Module.Markov
import System.Directory
import System.Environment
import System.FilePath
import System.Random

channels :: [Channel]
channels = ["#jlebot-test","#ucsd","#haskell-auto"]

saveFolder :: FilePath
saveFolder = "data"

main :: IO ()
main = do
    mode <- getArgs
    createDirectoryIfMissing True saveFolder
    chatbot' <- chatbot <$> newStdGen
    case mode of
      "irc":_ -> withIrc "irc.freenode.org"
                         "jlebot2"
                         channels
                         True
                         1000000
                         (chatbot' "irc")
      _       -> stdinLoopChron "justin"
                                "#stdin"
                                1000000
                                (chatbot' "stdin")

chatbot :: StdGen -> FilePath -> ChatBot IO
chatbot g rt = mconcat [ "karma"  <~ fromRoom karmaBot
                       , "greet"  <~ fromRoom (greetBot g)
                       , fromRoom (markovBot (pth "markov") g)
                       ]
  where
    pth ext = saveFolder </> (rt ++ "-" ++ ext)
    ext <~ cb = serializing' (pth ext) cb


