module Metainfo where

import BEncode;
import Data.Maybe;
import Network.URI (parseURI)
import Network.HTTP;
import qualified Data.Map as M;
import qualified Data.ByteString as B;
import qualified Crypto.Hash.SHA1 as SHA1;

data Metainfo = Metainfo {
  info :: M.Map String BData,
  announce :: B.ByteString,
  created :: Int,
  comment :: B.ByteString,
  author :: B.ByteString,
  encoding :: B.ByteString
}

infoHash :: Metainfo -> B.ByteString
infoHash metainfo = do
  SHA1.hash $ assumeBString $ fromJust $ M.lookup "pieces" $ info metainfo

metainfoFromFile :: String -> IO Metainfo
metainfoFromFile filename = do
  contents <- B.readFile filename
  return $ metainfoFromString contents

metainfoFromURL :: String -> IO Metainfo
metainfoFromURL url = do
  h <- simpleHTTP . defaultGETRequest_ . fromJust . parseURI $ url
  contents <- getResponseBody h
  return $ metainfoFromString contents

metainfoFromString :: B.ByteString -> Metainfo
metainfoFromString contents = Metainfo {
    info = assumeBDictionary $ getKey "info",
    announce = assumeBString $ getKey "announce",
    created = assumeBInteger $ getKey "created",
    comment = assumeBString $ getKey "comment",
    author = assumeBString $ getKey "author",
    encoding = assumeBString $ getKey "encoding"
  }
    where dict = decodeBDictionary contents
          getKey str = fromJust $ M.lookup str dict
