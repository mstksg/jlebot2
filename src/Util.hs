module Util (
    sealRandom
  ) where

import Control.Auto
import Control.Auto.Effects
import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Serialize
import Prelude hiding            ((.), id)

sealRandom :: (Monad m, RandomGen g, Serialize g)
           => Auto (RandT g m) a b
           -> g
           -> Auto m a b
sealRandom = sealState . hoistA (StateT . runRandT)
