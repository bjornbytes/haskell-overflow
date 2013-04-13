module Metainfo where

import BEncode;
import Data.Maybe;
import qualified Data.Map as M;
import qualified Data.ByteString as B;

data Metainfo = Metainfo {
      location :: String,
      
      info :: M.Map String BData,
      announceList :: [[String]],
      created :: Int,
      comment :: String,
      author :: String,
      encoding :: String
} deriving Show

metainfoFromFile :: String -> IO ()
metainfoFromFile filename = do
  contents <- B.readFile filename
  print $ fromJust $ M.lookup (BString "piece length") (assumeBDictionary $ fromJust $ M.lookup (BString "info") (decodeBDictionary contents))
  --print $ fromJust $ M.lookup (BString "") (decodeBDictionary contents)
  --print $ decodeBDictionary contents