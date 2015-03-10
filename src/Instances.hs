module Instances where

import Control.Applicative
import Data.Serialize
import Data.Text
import Data.Text.Encoding
import Data.Time
import System.Random

instance Serialize UTCTime where
    get = read <$> get
    put = put . show

instance Serialize Day where
    get = read <$> get
    put = put . show

instance Serialize StdGen where
    get = read <$> get
    put = put . show

instance Serialize Text where
    get = decodeUtf8 <$> get
    put = put . encodeUtf8
