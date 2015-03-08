module Main where

import System.Environment
import Auto.Chatbot.Backend.Stdin
import Auto.Chatbot.Backend.IRC
import Module.Karma
import Auto.Chatbot

channels :: [Channel]
channels = ["#jlebot-test","#ucsd","#haskell-auto"]

main :: IO ()
main =
    withIrc "irc.freenode.org" "jlebot2" channels True 1000000 chatbot
    -- stdinLoopChron "justin" "#stdin" 1000000 chatbot

chatbot :: Monad m => ChatBot m
chatbot = fromRoom karmaBot



