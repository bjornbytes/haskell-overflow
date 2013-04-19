module Main where

import qualified Metainfo as M
import Announcer
import Torrent
import qualified Network.HTTP.Types.URI as U;

main :: IO ()
main = do
  t <- torrentFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
  announce $ announcer t
  --print  $ U.urlEncode True $ infoHash $ metainfo t
  
