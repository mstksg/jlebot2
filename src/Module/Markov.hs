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
import Data.List hiding          (sum)
import Data.Map.Strict           (Map)
import Data.Maybe
import Instances                 ()
import Prelude hiding            ((.), id, sum)
import Text.Read                 (readMaybe)
import Util
import qualified Data.Map.Strict as M

type Training = Map String (Map Char Int)

memory :: Int
memory = 5

minLength :: Int
minLength = 15

maxTries :: Int
maxTries = 1000

markovBot :: MonadIO m => FilePath -> StdGen -> ChatBotRoom m
markovBot fp = sealRandom markovBotRandom
  where
    nfp = fp
    rfp = fp ++ "-rooms"
    markovBotRandom :: (MonadIO m, MonadRandom m) => ChatBotRoom m
    markovBotRandom = proc (InMessage nick msg room _) -> do
      -- serializing the map alone instead of the whole structure, in case
      -- we want to modify markovBot and don't want to lose the history
      nickTrain <- serializing' nfp (gather (const train)) -< (nick, msg)
      roomTrain <- serializing' rfp (gather (const train)) -< (room, msg)

      -- queryBlip :: Blip (Maybe Nick, Maybe Int)
      -- if the first field is Nothing, then ask for the room.
      -- the second field specifies a "minimum length"
      queryBlip <- emitJusts markovCommand -< msg

      let queryMap :: Maybe Nick -> (Maybe Training, Nick)
          -- markov for single user
          queryMap (Just qry) =
              ( mfilter (not . M.null) (M.lookup qry nickTrain)
              , qry )
          -- markov for global database
          queryMap Nothing    =
              ( mfilter (not . M.null) (M.lookup room roomTrain)
              , room )


      perBlip lookupResult -< first queryMap <$> queryBlip

    train :: Monad m
          => Interval m Message Training
    train = toOn . accum addToMap M.empty
      where
        addToMap oldMap message = M.unionWith squishMaps oldMap scoresMap
          where
            pairs = [ (s, M.singleton c 1) | (s, c) <- pairsToAdd message ]
            scoresMap = M.fromListWith squishMaps pairs

    squishMaps :: (Num a, Ord k) => Map k a -> Map k a -> Map k a
    squishMaps = M.unionWith (+)

    lookupResult :: MonadRandom m
                 => Auto m
                         ((Maybe Training, Nick), Maybe Int)
                         [Message]
    lookupResult = proc ((training, nick), minL) -> do
        let minLength' = fromMaybe minLength minL
        case training of
          Nothing ->
            id -< ["No data found for " ++ nick ++ "."]
          Just tr -> do
            result <- arrM (uncurry genMarkovN) -< (minLength', tr)
            id -< ["<" ++ nick ++ "> " ++ result]



-- Filter and parse a command.  The first field is a Maybe user; Nothing
-- means a request for the room.  The second field is the minimum character
-- length.
markovCommand :: Message -> Maybe (Maybe Nick, Maybe Int)
markovCommand message =
    case words message of
      "@markov":i:_           | isJust (readMaybe i :: Maybe Int)
                                    -> Just (Nothing, readMaybe i)
      ["@markov"]             -> Just (Nothing, Nothing)
      ["@markov",nick]        -> Just (Just nick, Nothing)
      ["@impersonate",nick]   -> Just (Just nick, Nothing)
      "@markov":nick:i:_      -> Just (Just nick, readMaybe i)
      "@impersonate":nick:i:_ -> Just (Just nick, readMaybe i)
      _                       -> Nothing

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

genMarkovN :: MonadRandom m => Int -> Training -> m String
genMarkovN i m = go maxTries
  where
    go 0 = genMarkov m
    go n = do
      mkv <- genMarkov m
      if length mkv >= i
        then return mkv
        else go (n-1)

genMarkov :: MonadRandom m => Training -> m String
genMarkov m = evalStateT (unfoldM go) ""
  where
    go :: MonadRandom m => StateT String m (Maybe Char)
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

