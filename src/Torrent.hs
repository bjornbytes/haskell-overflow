module Torrent where

import qualified Metainfo as M
import Announcer
import Peer
import System.IO.Unsafe
import Data.IORef

--Try to remove the following
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network
import Network.Socket
import System.IO
import Control.Concurrent

import Config
import Protocol

data Torrent = Torrent {
  metainfo :: M.Metainfo,
  announcer :: Announcer,
  inactivePeers :: IORef [InactivePeer],
  activePeers :: IORef [ActivePeer]
}

start :: Torrent -> IO ()
start t = do
  peers <- announce "started" $ announcer t
  sequence $ map (forkIO . torrentHandshake) peers 
  return ()

torrentHandshake :: Torrent
                 -> InactivePeer
                 -> IO ()
torrentHandshake torrent peer = do
 	handle <- connectTo (ip peer) (PortNumber $ fromIntegral $ port peer)
 	hSetBuffering handle NoBuffering
 	sendHandshake handle (infoHash $ metainfo torrent) (C.pack $ peerIdPrefix ++ "353535353535")
 	activePeer <- recvHandshake handle
 	addActivePeer activePeer $ activePeers torrent
 	torrentMain torrent activePeer

torrentMain :: Torrent
            -> ActivePeer
            -> IO ()
torrentMain = forever $ do
	msg@(header, payload) <- recvMessage
	C.putStrLn $ "Received " ++ header
	processMessage msg

addInactivePeer :: InactivePeer -> IORef [InactivePeer] -> IO ()
addInactivePeer p ps = do
  modifyIORef ps (p:)

addActivePeer :: ActivePeer -> IORef [ActivePeer] -> IO ()
addActivePeer p ps = do
  modifyIORef ps (p:)

torrentFromFile :: String -> IO Torrent
torrentFromFile filename = do
  m <- M.metainfoFromFile filename
  inactives <- newIORef []
  actives <- newIORef []
  return Torrent {
    metainfo = m,
    announcer = defaultAnnouncer m,
    inactivePeers = inactives,
    activePeers = actives
  }

torrentFromURL :: String -> IO Torrent
torrentFromURL url = do
  m <- M.metainfoFromURL url
  inactives <- newIORef []
  actives <- newIORef []
  return Torrent {
    metainfo = m,
    announcer = defaultAnnouncer m,
    inactivePeers = inactives,
    activePeers = actives
  }

defaultAnnouncer :: M.Metainfo -> Announcer
defaultAnnouncer m = Announcer {
  url = M.announce m,
  infoHash = M.infoHash m,
  uploaded = 0,
  downloaded = 0,
  left = M.infoLength m
}
