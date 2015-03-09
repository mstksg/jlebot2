module Main where

import Auto.Chatbot
import Auto.Chatbot.Backend.IRC
import Auto.Chatbot.Backend.Stdin
import Data.Monoid
import Module.Greet
import Module.Karma
import System.Environment

channels :: [Channel]
channels = ["#jlebot-test","#ucsd","#haskell-auto"]

main :: IO ()
main =
    withIrc "irc.freenode.org" "jlebot2" channels True 1000000 chatbot
    -- stdinLoopChron "justin" "#stdin" 1000000 chatbot

chatbot :: Monad m => ChatBot m
chatbot = mconcat [ fromRoom karmaBot
                  , fromRoom greetBot
                  ]


