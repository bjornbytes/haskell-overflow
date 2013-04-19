module Main where

import qualified Metainfo as M
import Peer
import Announcer
import Torrent
import qualified Network.HTTP.Types.URI as U;
import qualified Data.ByteString as B;
import qualified Data.ByteString.Char8 as C;
import Data.Char (ord)

main :: IO ()
main = do
  t <- torrentFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
  ps <- announce "started" $ announcer t
  print $ ps
