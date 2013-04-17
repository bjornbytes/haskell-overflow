module Torrent where

import Metainfo
import Peer

data Torrent = Torrent {
  metainfo :: Metainfo,
  peers :: [Peer]
}

torrentFromFile :: String -> IO Torrent
torrentFromFile filename = do
  m <- metainfoFromFile filename
  return Torrent {
    metainfo = m,
    peers = []
  }

torrentFromURL :: String -> IO Torrent
torrentFromURL url = do
  m <- metainfoFromURL url
  return Torrent {
    metainfo = m,
    peers = []
  }

