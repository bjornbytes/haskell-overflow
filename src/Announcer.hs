module Announcer where

import BEncode
import qualified Peer as P
import Data.Maybe
import Network.URI (parseURI)
import Network.HTTP
import qualified Network.HTTP.Types.URI as U
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M;

data Announcer = Announcer {
  url :: B.ByteString,
  
  infoHash :: B.ByteString,
  peerId :: B.ByteString,
  localPort :: Int,
  uploaded :: Int,
  downloaded :: Int,
  left :: Int
}

announce :: String -> Announcer -> IO [P.Peer]
announce event a = do
  putStrLn $ announceRequest event a
  h <- simpleHTTP $ defaultGETRequest_ $ fromJust $ parseURI $ announceRequest event a
  response <- getResponseBody h
  return $ parsePeers $ assumeBString $ fromJust $ M.lookup "peers" $ decodeBDictionary response

announceRequest :: String -> Announcer -> String
announceRequest event a = C.unpack $ B.append (url a) $ B.concat
                      [ C.pack "?info_hash=", U.urlEncode True $ infoHash a
                      , C.pack "&peer_id=", U.urlEncode True $ peerId a
                      , C.pack "&port=", C.pack $ show $ localPort a
                      , C.pack "&uploaded=", C.pack $ show $ uploaded a
                      , C.pack "&downloaded=", C.pack $ show $ downloaded a
                      , C.pack "&left=", C.pack $ show $ left a
                      , C.pack "&compact=1" ]

parsePeers :: B.ByteString -> [P.Peer]
parsePeers str = case B.length str of
                   0 -> []
                   otherwise -> p:(parsePeers rest)
                                   where p = P.Peer {
                                               P.ip = C.unpack $ B.take 4 piece,
                                               P.port = ((fromIntegral $ B.head $ B.drop 4 piece) * 256) + (fromIntegral $ B.head $ B.drop 5 piece),

                                               P.interested = False,
                                               P.interesting = False,
                                               P.choked = True,
                                               P.choking = True
                                             }
                                         (piece, rest) = B.splitAt 6 str
