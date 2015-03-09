module Instances where

import Control.Applicative
import Data.Serialize
import System.Random
import Data.Time

instance Serialize UTCTime where
    get = read <$> get
    put = put . show

instance Serialize StdGen where
    get = read <$> get
    put = put . show
