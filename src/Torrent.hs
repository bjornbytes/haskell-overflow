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

data Torrent = Torrent {
  metainfo :: M.Metainfo,
  announcer :: Announcer,
  inactivePeers :: IORef [InactivePeer],
  activePeers :: IORef [ActivePeer]
}

start :: Torrent -> IO ()
start t = do
  peers <- announce "started" $ announcer t
  sequence $ map (forkIO . initiateHandshake t) peers 
  return ()

slice2 :: Int -> Int -> B.ByteString -> B.ByteString
slice2 start length str = B.take length $ snd $ B.splitAt start str

initiateHandshake :: Torrent -> InactivePeer -> IO ()
initiateHandshake t p = do
  putStrLn $ "Connecting to " ++ (ip p) ++ ":" ++ (show $ port p)
  handle <- connectTo (ip p) (PortNumber $ fromIntegral $ port p)
  hSetBuffering handle NoBuffering
  C.hPutStr handle buildHandshakeString
 
  putStrLn "Sent handshake, waiting for response"
  line <- B.hGetLine handle --Wait for a response
  C.putStrLn $ line
  let pLen = read $ show $ B.head line
  let protocol = slice2 1 pLen line
  case C.unpack protocol of
    "BitTorrent protocol" -> do
        addActivePeer peer $ activePeers t
        putStrLn $ "Client connected using protocol p" ++ (show protocol)
        forkIO $ handleMessage handle peer t
        return ()
        where peer = ActivePeer {
                       peerId = slice2 (1 + pLen + 8 + 20) 20 line,
                       credentials = undefined,
                       
                       interested = False,
                       interesting = False,
                       choking = True,
                       choked = True
                     }
    otherwise -> putStrLn $ "Unsupported protocol: " ++ (show protocol)
  return ()
    where buildHandshakeString = B.concat
                                 [ B.singleton (fromIntegral 19)
                                 , C.pack "BitTorrent protocol"
                                 , B.replicate 8 (fromIntegral 0)
                                 , M.infoHash $ metainfo t
                                 , C.pack "-OF0001-353535353535"
                                 ]

handleMessage :: Handle -> ActivePeer -> Torrent -> IO ()
handleMessage handle p t = do
  line <- hGetLine handle
  putStrLn $ "Received " ++ line

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
  localPort = 6060,
  uploaded = 0,
  downloaded = 0,
  left = M.infoLength m
}
