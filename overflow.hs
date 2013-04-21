module Main where

import BEncode
import qualified Metainfo as M
import Peer
import Announcer
import Torrent
import TorrentManager
import SocketManager
import qualified Network.HTTP.Types.URI as U;
import qualified Data.ByteString as B;
import qualified Data.ByteString.Char8 as C;
import Data.Char (ord)
import qualified Data.Map as Map;
import Data.Maybe
import Control.Concurrent
import Control.Monad

import Config

main :: IO ()
main = do
  done <- newEmptyMVar
  forkIO $ listenOn localPort done
  t <- addTorrentFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
  start t
  takeMVar done
