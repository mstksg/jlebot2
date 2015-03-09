module Main where

import Auto.Chatbot
import Auto.Chatbot.Backend.IRC
import Auto.Chatbot.Backend.Stdin
import Control.Applicative
import Data.Monoid
import Module.Greet
import Module.Karma
import System.Environment
import System.Random

channels :: [Channel]
channels = ["#jlebot-test","#ucsd","#haskell-auto"]

main :: IO ()
main = do
    mode <- getArgs
    chatbot' <- chatbot <$> newStdGen
    case mode of
      "irc":_ -> withIrc "irc.freenode.org" "jlebot2" channels True 1000000 chatbot'
      _       -> stdinLoopChron "justin" "#stdin" 1000000 chatbot'

chatbot :: Monad m => StdGen -> ChatBot m
chatbot g = mconcat [ fromRoom karmaBot
                    , fromRoom (greetBot g)
                    ]


