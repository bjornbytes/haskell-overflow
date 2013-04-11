module Metainfo where

import BEncode;
import Data.Map;

data Metainfo = Metainfo {
      location :: String,
      
      info :: Map String BData,
      announceList :: [[String]],
      created :: Int,
      comment :: String,
      author :: String,
      encoding :: String
} deriving Show