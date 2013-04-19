module Torrent where

import qualified Metainfo as M
import Announcer
import Peer

data Torrent = Torrent {
  metainfo :: M.Metainfo,
  announcer :: Announcer,
  peers :: [Peer]
}

torrentFromFile :: String -> IO Torrent
torrentFromFile filename = do
  m <- M.metainfoFromFile filename
  return Torrent {
    metainfo = m,
    announcer = defaultAnnouncer m,
    peers = []
  }

torrentFromURL :: String -> IO Torrent
torrentFromURL url = do
  m <- M.metainfoFromURL url
  return Torrent {
    metainfo = m,
    announcer = defaultAnnouncer m,
    peers = []
  }

defaultAnnouncer :: M.Metainfo -> Announcer
defaultAnnouncer m = Announcer {
  url = M.announce m,
  infoHash = M.infoHash m,
  peerId = M.infoHash m,
  localPort = 6881,
  uploaded = 0,
  downloaded = 0,
  left = 0
}
