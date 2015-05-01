{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Module.Zzz (zzzBot) where

import Auto.Chatbot
import Control.Monad.Random
import Control.Lens
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Effects
import Control.Auto.Process.Random
import Data.Map.Strict             (Map)
import Prelude hiding              ((.), id)
import Text.Read
import qualified Data.Map.Strict   as M

data ZzzComm = ZCAdd Message
             | ZCList
             | ZCDelete (Maybe Int)
             | ZCError String
             | ZCZzz

makePrisms ''ZzzComm

helpMsg :: Message
helpMsg = "[zzz] zzz; [add] zzz+ msg; [del] zzz- #; [del all] zzz- all; [help] zzz?"

initialZzzs :: Map Nick [Message]
initialZzzs = M.fromList [ ("jle`", [ "...did you expect me to tuck you in or something"
                                    , "good night oh captain my captain"
                                    , "chú ơi chú ngủ ngon nhé"
                                    ]
                           )
                         , ("rray", ["zzz"])
                         ]
zzzBot :: Monad m => StdGen -> ChatBotRoom m
zzzBot = sealRandomStd zzzBot'
  where
    zzzBot' :: MonadRandom m => ChatBotRoom m
    zzzBot' = proc (InMessage nick msg src _) -> do
      cmdB <- emitJusts parseCommand       -< msg
      zzzs <- scanB updateZzz initialZzzs  -< (nick,) <$> cmdB

      listB <- mapMaybeB (preview _ZCList)   -< cmdB
      addB  <- mapMaybeB (preview _ZCAdd)    -< cmdB
      delB  <- mapMaybeB (preview _ZCDelete) -< cmdB
      zzzB  <- mapMaybeB (preview _ZCZzz)    -< cmdB

      let userZzz = M.findWithDefault [] nick zzzs
          listMsg | nick == src = zipWith mkList [1..] userZzz
                               ++ [helpMsg]
                  | otherwise   = [ helpMsg
                                  , "'zzz?' in pm to view registered zzz's"]

      zzzB' <- arrMB uniform . filterB (not . null) -< userZzz <$ zzzB

      let zzzB'' = fmap (\msg' -> [nick ++ ": " ++ msg']) zzzB'

      id -< zzzB''
         <> (listMsg             <$ listB)
         <> (["zzz Added!"]      <$ addB)
         <> (["zzz(s) Deleted!"] <$ delB)

    parseCommand :: Message -> Maybe ZzzComm
    parseCommand str = case words str of
                         "zzz+":toAdd@(_:_) -> Just (ZCAdd (unwords toAdd))
                         "zzz?":_           -> Just ZCList
                         "zzz-":"all":_     -> Just (ZCDelete Nothing)
                         "zzz-":i:_         -> Just $ case readMaybe i of
                                                 Just i' -> ZCDelete (Just i')
                                                 Nothing -> ZCError "Bad number."
                         "zzz":_            -> Just ZCZzz
                         _                  -> Nothing
    updateZzz :: Map Nick [Message] -> (Nick, ZzzComm) -> Map Nick [Message]
    updateZzz m (nick, comm) =
        case comm of
          ZCAdd msg         -> M.insertWith (++) nick [msg] m
          ZCDelete (Just i) -> M.update (killEmpty . dropAt (i - 1)) nick m
          ZCDelete Nothing  -> M.delete nick m
          _                 -> m
    killEmpty :: [a] -> Maybe [a]
    killEmpty [] = Nothing
    killEmpty xs = Just xs
    dropAt :: Int -> [a] -> [a]
    dropAt n xs = case splitAt n xs of
                    (befs,[])   -> befs
                    (befs,_:ys) -> befs ++ ys
    mkList :: Int -> String -> String
    mkList i msg = "[" ++ show i ++ "] " ++ msg


