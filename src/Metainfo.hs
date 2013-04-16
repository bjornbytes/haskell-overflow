module Metainfo where

import BEncode;
import Data.Maybe;
import qualified Data.Map as M;
import qualified Data.ByteString as B;

data Metainfo = Metainfo {
      location :: String,
      
      info :: M.Map BData BData,
      announce :: String,
      created :: Int,
      comment :: String,
      author :: String,
      encoding :: String
} deriving Show

metainfoFromFile :: String -> IO Metainfo
metainfoFromFile filename = do
  contents <- B.readFile filename
  return Metainfo {
    location = filename,
    
    info = assumeBDictionary $ fromJust $ M.lookup (BString "info") (decodeBDictionary contents),
    announce = assumeBString $ fromJust $ M.lookup (BString "announce") (decodeBDictionary contents),
    created = assumeBInteger $ fromJust $ M.lookup (BString "created") (decodeBDictionary contents),
    comment = assumeBString $ fromJust $ M.lookup (BString "comment") (decodeBDictionary contents),
    author = assumeBString $ fromJust $ M.lookup (BString "author") (decodeBDictionary contents),
    encoding = assumeBString $ fromJust $ M.lookup (BString "encoding") (decodeBDictionary contents)
  }
  
  --print $ fromJust $ M.lookup (BString "piece length") (assumeBDictionary $ fromJust $ M.lookup (BString "info") (decodeBDictionary contents))
  --print $ fromJust $ M.lookup (BString "") (decodeBDictionary contents)
  --print $ decodeBDictionary contents