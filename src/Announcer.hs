module Announcer where

import BEncode
import Data.Maybe
import Network.URI (parseURI)
import Network.HTTP
import qualified Network.HTTP.Types.URI as U
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Announcer = Announcer {
  url :: B.ByteString,
  
  infoHash :: B.ByteString,
  peerId :: B.ByteString,
  localPort :: Int,
  uploaded :: Int,
  downloaded :: Int,
  left :: Int
}

announce :: Announcer -> IO ()
announce a = do
  C.putStrLn $ announceRequest a ""
  h <- simpleHTTP . defaultGETRequest_ . fromJust . parseURI $ C.unpack $ announceRequest a ""
  response <- getResponseBody h
  C.putStrLn response

announceRequest :: Announcer -> String -> B.ByteString
announceRequest a event = B.append (url a) $ B.concat [C.pack "?info_hash=", U.urlEncode True $ infoHash a, C.pack "&peer_id=", U.urlEncode True $ peerId a, C.pack "&port=", C.pack $ show $ localPort a, C.pack "&uploaded=", C.pack $ show $ uploaded a, C.pack "&downloaded=", C.pack $ show $ downloaded a, C.pack "&left=", C.pack $ show $ left a, C.pack "&compact=1"]
