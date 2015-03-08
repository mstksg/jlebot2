module Main where

import System.Environment
import Backend.StdIn
import Module.Karma
import Types

main :: IO ()
main = do
    modes <- getArgs
    if "irc" `elem` modes
      then undefined
      else stdinLoop chatbot

chatbot :: Monad m => ChatBot m
chatbot = fromRoom karmaBot



