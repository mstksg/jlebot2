{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Module.Markov (markovBot) where

import Auto.Chatbot
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Interval
import Control.Auto.Serialize
import Control.Monad             (mfilter)
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List
import Data.Map.Strict           (Map)
import Instances                 ()
import Prelude hiding            ((.), id)
import Util
import qualified Data.Map.Strict as M

type Training = Map String (Map Char Int)

memory :: Int
memory = 5

minLength :: Int
minLength = 10

maxTries :: Int
maxTries = 50

markovBot :: MonadIO m => FilePath -> StdGen -> ChatBotRoom m
markovBot fp = sealRandom markovBotRandom
  where
    markovBotRandom :: MonadIO m => ChatBotRoom (RandT StdGen m)
    markovBotRandom = proc (InMessage nick msg _ _) -> do
      trainings <- serializing' fp $ gather (const train) -< (nick, msg)

      queryBlip <- emitJusts markovCommand -< msg

      let queryMap :: Nick -> (Maybe Training, Nick)
          queryMap qry = ( mfilter (not . M.null) (M.lookup qry trainings)
                         , qry)

      perBlip lookupResult -< queryMap <$> queryBlip

    train :: Monad m
          => Interval m Message Training
    train = toOn . accum addToMap M.empty
      where
        addToMap oldMap message = M.unionWith squishMaps oldMap scoresMap
          where
            pairs = [ (s, M.singleton c 1) | (s, c) <- pairsToAdd message ]
            scoresMap = M.fromListWith squishMaps pairs
        squishMaps = M.unionWith (+)

    lookupResult :: Monad m
                 => Auto (RandT StdGen m) (Maybe Training, Nick) [Message]
    lookupResult = proc (training, nick) ->
        case training of
          Nothing ->
            id -< ["No data found for " ++ nick ++ "."]
          Just tr -> do
            result <- arrM (genMarkovN minLength) -< tr
            id -< [result]



    
markovCommand :: Message -> Maybe Nick
markovCommand message = case words message of
                          "@markov":nick:_      -> Just nick
                          "@impersonate":nick:_ -> Just nick
                          _                     -> Nothing

pairsToAdd :: String -> [(String, Char)]
pairsToAdd ('@':_)     = []
pairsToAdd ('>':'>':_) = []
pairsToAdd ('.':_)     = []
pairsToAdd ('s':'/':_) = []
pairsToAdd xs          = map pullLast . transpose
                       . dropchain . ('\0':) . reverse
                       $ xs
  where
      pullLast []     = ([],'\0')
      pullLast (y:ys) = (reverse ys, y)
      dropchain ys    = map (`drop` ys) [0..memory]

genMarkovN :: Monad m => Int -> Training -> RandT StdGen m String
genMarkovN i m = go maxTries
  where
    go 0 = genMarkov m
    go n = do
      mkv <- genMarkov m
      if length mkv >= i
        then return mkv
        else go (n-1)

genMarkov :: Monad m => Training -> RandT StdGen m String
genMarkov m = evalStateT (unfoldM go) ""
  where
    go :: Monad m => StateT String (RandT StdGen m) (Maybe Char)
    go = do
      options <- gets (\s -> M.findWithDefault M.empty s m)
      let ml = M.toList options
      if null ml
        then return Nothing
        else do
          n <- lift . fromList . map (second fromIntegral) $ ml
          case n of
            '\0' ->
              return Nothing
            c    -> do
              modify $ reverse . take memory . reverse . (++ [c])
              return (Just c)

