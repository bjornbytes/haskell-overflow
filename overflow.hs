module Main where

import BEncode
import qualified Metainfo as M
import Peer
import Announcer
import Torrent
import Network
import qualified Network.HTTP.Types.URI as U;
import qualified Data.ByteString as B;
import qualified Data.ByteString.Char8 as C;
import Data.Char (ord)
import qualified Data.Map as Map;
import Data.Maybe

peerId :: B.ByteString
peerId = C.pack "-OF0001-353535353535"

main :: IO ()
main = do
  t <- torrentFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
  putStrLn $ "Downloading " ++ (C.unpack $ assumeBString $ fromJust $ Map.lookup "name" $ M.info $ metainfo t) ++ " [" ++ (show $ assumeBInteger $ fromJust $ Map.lookup "length" $ M.info $ metainfo t) ++ " bytes]..."
  sock <- listenOn $ PortNumber 6888
  putStrLn "Listening on 6888..."
  socketAccept sock

socketAccept :: Socket -> IO ()
socketAccept sock = do
  (handle, ip, port) <- accept sock
  putStrLn $ "New connection from " ++ (show ip) ++ ":" ++ (show port)
  --Do stuff
  socketAccept sock
