{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Module.Karma (karmaBot) where

import Auto.Chatbot
import Control.Auto
import Control.Auto.Blip
import Data.Foldable
import Data.List
import Prelude hiding            ((.), id)
import qualified Data.Map.Strict as M

karmaBot :: Monad m => ChatBotRoom m
karmaBot = proc (InMessage nick msg _ _) -> do
    karmaIncreases <- emitJusts (grabKarmas "++") -< msg
    karmaDecreases <- emitJusts (grabKarmas "--") -< msg

    let karmaIncreases' = filter (/= nick) <$> karmaIncreases
        karmaUpdates    = merge (++) (map (,  1) <$> karmaIncreases')
                                     (map (, -1) <$> karmaDecreases )


    karmaMap <- holdWith M.empty . accumB addToMap M.empty -< karmaUpdates

    queryBlip <- emitJusts karmaCommand -< msg

    let lookupKarma :: Nick -> [Message]
        lookupKarma qry = let karma = M.findWithDefault 0 qry karmaMap
                          in  [ qry ++ " has a karma of "
                             ++ show karma ++ "."
                              ]

    id -< lookupKarma <$> queryBlip
  where
    addToMap :: M.Map Nick Int -> [(Nick, Int)] -> M.Map Nick Int
    addToMap mp updates = M.unionWith (+) mp (M.fromListWith (+) updates)

karmaCommand :: Message -> Maybe Nick
karmaCommand message = case words message of
                         "@karma":nick:_ -> Just nick
                         _               -> Nothing

grabKarmas :: String -> Message -> Maybe [Nick]
grabKarmas sign message = foldMap parseKarma (words message)
  where
    parseKarma word | sign `isSuffixOf` word = Just [word']
                    | otherwise              = Nothing
      where
        word' = reverse . drop (length sign) . reverse $ word
