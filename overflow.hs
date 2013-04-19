module Main where

import BEncode
import qualified Metainfo as M
import Peer
import Announcer
import Torrent
import SocketManager
import qualified Network.HTTP.Types.URI as U;
import qualified Data.ByteString as B;
import qualified Data.ByteString.Char8 as C;
import Data.Char (ord)
import qualified Data.Map as Map;
import Data.Maybe
import Control.Concurrent

main :: IO ()
main = do
  forkIO $ listenOn 6888
  t <- torrentFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
  putStrLn $ "Downloading " ++ (C.unpack $ assumeBString $ fromJust $ Map.lookup "name" $ M.info $ metainfo t) ++ " [" ++ (show $ assumeBInteger $ fromJust $ Map.lookup "length" $ M.info $ metainfo t) ++ " bytes]..."

