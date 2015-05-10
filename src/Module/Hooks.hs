module Module.Hooks (hookBot) where

import Auto.Chatbot
import Control.Auto
import Data.Traversable
import Control.Auto.Blip
import Control.Auto.Process.Random
import Control.Monad.Random
import Data.Map.Strict             (Map)
import Prelude hiding              ((.), id, mapM)
import qualified Data.Map.Strict   as M

hookBot :: Monad m => StdGen -> ChatBotRoom m
hookBot = sealRandomStd (modifyBlips (:[]) . onJusts . arrM f)
  where
    f (InMessage nick msg _ _) = mapM uniform $ uMsg <|> gMsg
      where
        uMsg = M.lookup (nick, msg) uHooks
        gMsg = M.lookup msg gHooks

uHooks :: Map (Nick, Message) [Message]
uHooks = M.fromList [ (("mike_pizza","cerealbot: beer++"), ["bro"])
                    ]


gHooks :: Map Message [Message]
gHooks = M.fromList [ ("@flip table", ["(╯°□°）╯︵ ┻━┻"])
                    , ("@flip table --all", ["┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻`"])
                    , ("@reset table", ["┬─┬ノ( º _ ºノ)"])
                    , ("@karma beer", ["bro"])
                    , ("rip", ["rip"])
                    , ("o/", ["\\o"])
                    ]
