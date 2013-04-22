module Metainfo where

import BEncode;
import Data.Maybe;
import Network.URI (parseURI)
import Network.HTTP;
import qualified Data.Map as M;
import qualified Data.ByteString as B;
import qualified Crypto.Hash.SHA1 as SHA1;

data Metainfo = Metainfo {
  announce :: B.ByteString,
  info :: BDict
}

infoHash :: Metainfo -> B.ByteString
infoHash m = do
	SHA1.hash $ encode $ BDictionary $ info m

infoLength :: Metainfo -> Int
infoLength m = assumeBInteger $ fromJust $ M.lookup "length" $ info m

infoPieceLength :: Metainfo -> Int
infoPieceLength m = assumeBInteger $ fromJust $ M.lookup "piece length" $ info m

infoPieceCount :: Metainfo -> Int
infoPieceCount m = (infoLength m `div` infoPieceLength m) + 1

metainfoFromFile :: String -> IO Metainfo
metainfoFromFile filename = do
  contents <- B.readFile filename
  return $ metainfoFromString contents

metainfoFromURL :: String -> IO Metainfo
metainfoFromURL url = do
  h <- simpleHTTP . defaultGETRequest_ . fromJust $ parseURI  url
  contents <- getResponseBody h
  return $ metainfoFromString contents

metainfoFromString :: B.ByteString -> Metainfo
metainfoFromString contents = case M.lookup "files" $ infoDictionary of
                                Nothing -> Metainfo {
                                             info = infoDictionary,
                                             announce = assumeBString $ getKey "announce"
                                           }
                                otherwise -> error "Overflow only supports single-file torrents."
    where 
      infoDictionary = assumeBDictionary $ getKey "info"
      dict = decodeBDictionary contents
      getKey str = fromJust $ M.lookup str dict

