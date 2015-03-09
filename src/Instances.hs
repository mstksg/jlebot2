module Instances where

import Control.Applicative
import Data.Serialize
import Data.Time

instance Serialize UTCTime where
    get = read <$> get
    put = put . show

