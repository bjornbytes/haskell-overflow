module Metainfo where

import BEncode;
import Data.Maybe;
import Network.URI (parseURI)
import Network.HTTP;
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

metainfoFromURL :: String -> IO Metainfo
metainfoFromURL url = do
  h <- simpleHTTP $ defaultGETRequest_ $ fromJust $ parseURI url
  contents <- getResponseBody h
  return Metainfo {
    location = url,
    
    info = assumeBDictionary $ fromJust $ M.lookup (BString "info") (decodeBDictionary contents),
    announce = assumeBString $ fromJust $ M.lookup (BString "announce") (decodeBDictionary contents),
    created = assumeBInteger $ fromJust $ M.lookup (BString "created") (decodeBDictionary contents),
    comment = assumeBString $ fromJust $ M.lookup (BString "comment") (decodeBDictionary contents),
    author = assumeBString $ fromJust $ M.lookup (BString "author") (decodeBDictionary contents),
    encoding = assumeBString $ fromJust $ M.lookup (BString "encoding") (decodeBDictionary contents)
  }