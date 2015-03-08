module Main where

import System.Environment
import Auto.Chatbot.Backend.Stdin
import Module.Karma
import Auto.Chatbot

main :: IO ()
main = do
    modes <- getArgs
    stdinLoopChron "justin" "#stdin" 1000000 chatbot
    -- if "irc" `elem` modes
    --   then undefined
    --   else stdinLoop chatbot

chatbot :: Monad m => ChatBot m
chatbot = fromRoom karmaBot



