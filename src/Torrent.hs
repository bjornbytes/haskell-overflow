module Torrent where

import qualified Metainfo as M
import Announcer
import Peer
import System.IO.Unsafe
import Data.IORef
import Data.Array.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad

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
	sequence $ map (forkIO . torrentHandshake t) peers 
	return ()

torrentHandshake :: Torrent
                 -> InactivePeer
                 -> IO ()
torrentHandshake torrent peer = do
	putStrLn $ "Connecting to " ++ (show $ ip peer) ++ ":" ++ (show $ port peer)
 	handle <- connectTo (ip peer) (PortNumber $ fromIntegral $ port peer)
 	hSetBuffering handle LineBuffering
 	putStrLn $ "Sending handshake to " ++ (show $ ip peer) ++ ":" ++ (show $ port peer)
 	sendHandshake handle (M.infoHash $ metainfo torrent) (C.pack $ peerIdPrefix ++ "353535353535")
 	activePeer <- recvHandshake handle $ metainfo torrent
 	putStrLn $ "Received handshake from " ++ (show $ ip peer) ++ ":" ++ (show $ port peer)
 	addActivePeer activePeer $ activePeers torrent
 	torrentMain torrent activePeer

torrentMain :: Torrent
            -> ActivePeer
            -> IO ()
torrentMain torrent peer = forever $ do #AE81DF
	msg@(header, payload) <- recvMessage (prHandle peer)
	processMessage peer msg

processMessage :: ActivePeer
               -> (MsgHeader, [B.ByteString])
               -> IO ()
processMessage peer (header, payload) = 
	case header of
		MsgChoke -> writeIORef (prChoking peer) True
		MsgUnchoke -> writeIORef (prChoking peer) False
		MsgInterested -> writeIORef (prInterested peer) True
		MsgUninterested -> writeIORef (prInterested peer) False
		MsgHave -> do
			writeArray (prPieces peer) (readInt $ head payload) True
			return ()
		MsgBitfield -> do

			-- Parse the payload into a list of Bools, then make an assocs call.

			return ()
		MsgRequest -> do
			let [pieceIdx, offset, len] = payload

			-- Check if I have the requested block of the requested piece.  If so, send a MsgPiece message.

			return ()
		MsgPiece -> do
			let [pieceIdx, offset, content] = payload

			-- Write the content to the file, at byte position (pieceIdx * pieceSize + offset)
			-- Update our 'local bitfield'.  If we completed a piece, hash check it and broadcast a MsgHave.

			return ()
		MsgCancel -> do
			let [pieceIdx, offset, len] = payload

			-- Make sure to never send the peer the specified block.
			
			return ()

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
