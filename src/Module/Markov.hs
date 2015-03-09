module Module.Markov (markovBot) where

import Data.Map.Strict           (Map)
import qualified Data.Map.Strict as M

type Training = Map String (Map Char Int)

markovBot :: Monad m => ChatBotRoom m
markovBot = undefined
