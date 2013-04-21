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
  localPort :: Int,
  uploaded :: Int,
  downloaded :: Int,
  left :: Int
}

announce :: String -> Announcer -> IO [P.InactivePeer]
announce event a = do
  h <- simpleHTTP $ defaultGETRequest_ $ fromJust $ parseURI $ announceRequest event a
  response <- getResponseBody h
  print response
  let peers = parsePeers $ assumeBString $ fromJust $ M.lookup "peers" $ decodeBDictionary response
  putStrLn $ "Announce returned " ++ (show $ length peers) ++ " peers:"
  sequence $ map (print . P.ip) peers
  return peers

announceRequest :: String -> Announcer -> String
announceRequest event a = C.unpack $ B.append (url a) $ B.concat
                      [ C.pack "?info_hash=", U.urlEncode True $ infoHash a
                      , C.pack "&peer_id=", U.urlEncode True $ C.pack "-OF0001-353535353535"
                      , C.pack "&port=", C.pack $ show $ localPort a
                      , C.pack "&uploaded=", C.pack $ show $ uploaded a
                      , C.pack "&downloaded=", C.pack $ show $ downloaded a
                      , C.pack "&left=", C.pack $ show $ left a
                      , C.pack "&compact=1" ]

parsePeers :: B.ByteString -> [P.InactivePeer]
parsePeers str = case B.length str of
                   0 -> []
                   otherwise -> p:(parsePeers rest)
                                   where p = P.InactivePeer {
                                               P.ip = convertIp $ B.take 4 piece,
                                               P.port = ((fromIntegral $ B.head $ B.drop 4 piece) * 256) + (fromIntegral $ B.head $ B.drop 5 piece)
                                             }
                                         (piece, rest) = B.splitAt 6 str
                                         convertIp str = C.unpack $ B.tail $ B.concatMap (C.pack . (++) "." . show) str
