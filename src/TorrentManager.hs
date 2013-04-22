module TorrentManager where

import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.IO
import Network
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import Data.Maybe

import Metainfo
import Torrent
import Peer

--Th-There must be another way...
torrentMap :: IORef (Map.Map B.ByteString Torrent)
torrentMap = unsafePerformIO $ newIORef Map.empty

addTorrentFromURL :: String -> IO Torrent
addTorrentFromURL url = do
  t <- torrentFromURL url
  modifyIORef torrentMap (Map.insert (infoHash $ metainfo t) t)
  return t

addTorrentFromFile :: String -> IO Torrent
addTorrentFromFile filename = do
  t <- torrentFromFile filename
  modifyIORef torrentMap (Map.insert (infoHash $ metainfo t) t)
  return t

--SocketManager calls this when the first message is received from a new client.
handshakeHandler :: Handle -> IO ()
handshakeHandler handle = do
  putStrLn "Todo: Accept new connections correctly."
	{--addActivePeer peer $ activePeers torrent
  putStrLn $ "Client connected using protocol" ++ (show protocol)
  putMVar done ()
  messageHandler credentials peer torrent
  where torrent = fromJust $ Map.lookup (slice (1 + pLen + 8) 20 line) tMap
        peer = ActivePeer {
                 peerId = slice (1 + pLen + 8 + 20) 20 line,
                 prHandle = handle,
                 
                 interested = False,
                 interesting = False,
                 choking = True,
                 choked = True
                     }--}

messageHandler :: (Handle, HostName, PortNumber) -> ActivePeer -> Torrent -> IO ()
messageHandler credentials@(handle, ip, port) peer torrent = do
  line <- hGetLine handle
  putStrLn line
  messageHandler credentials peer torrent

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start length str = B.take length $ snd $ B.splitAt start str
