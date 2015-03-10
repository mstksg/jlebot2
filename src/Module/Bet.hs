{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ViewPatterns #-}

module Module.Bet (betBot) where

import Auto.Chatbot
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Data.Either
import Data.Map.Strict           (Map)
import Data.Time
import Instances                 ()
import Prelude hiding            ((.), id)
import Text.Read
import qualified Data.Map.Strict as M

type BetId = Integer

data BetCommand = MakeBet [Nick] Integer
                | ReplyBet Nick Bool BetId
                | CheckBets Nick
                | ResolveBet Nick BetId
                | CancelBet Nick BetId
                | RecoverCredits

type Transaction = Map String Integer

startingCredits :: Integer
startingCredits = 100

betBot :: Monad m => ChatBotChronRoom m
betBot = proc input -> do
    cmd <- case input of
             Left time ->
               tagBlips RecoverCredits . onChange -< utctDay time
             Right (InMessage nick msg _ _) ->
               emitJusts getBet -< (msg, nick)
    bank <- accum updateBank M.empty -< M.empty :: Transaction

    id -< undefined
  where
    updateBank :: Map String Integer -> Transaction -> Map String Integer
    updateBank bank trans = let news  = M.difference trans bank
                                newsT = 100 <$ news
                            in  bank `addTrans` newsT `addTrans` trans

    addTrans :: Transaction -> Transaction -> Transaction
    addTrans = M.unionWith (+)


getBet :: (Message, Nick) -> Maybe BetCommand
getBet (words -> msgWords, src) =
    case msgWords of
      "@bet":"make":nick:amt:_ -> MakeBet [nick] <$> readMaybe amt
      "@bet":"accept":bid:_    -> ReplyBet src True <$> readMaybe bid
      "@bet":"reject":bid:_    -> ReplyBet src False <$> readMaybe bid
      "@bet":"check":nick:_    -> Just $ CheckBets nick
      "@bet":"resolve":bid:_   -> ResolveBet src <$> readMaybe bid
      "@bet":"cancel":bid:_    -> CancelBet src <$> readMaybe bid
      _                        -> Nothing
