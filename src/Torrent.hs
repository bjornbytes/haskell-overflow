module Torrent where

import Data.IORef
import Data.Array.IO
import Network
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Announcer
import Config
import Peer
import Piece
import Protocol
import qualified Metainfo as M

data Torrent = Torrent {
	metainfo :: M.Metainfo,
	announcer :: Announcer,
	inactivePeers :: IORef [InactivePeer],
	activePeers :: IORef [ActivePeer],

	pieces :: IOArray Int Piece
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
torrentMain torrent peer = forever $ do
	msg@(header, payload) <- recvMessage (prHandle peer)
	processMessage torrent peer msg

processMessage :: Torrent
               -> ActivePeer
               -> (MsgHeader, [B.ByteString])
               -> IO ()
processMessage torrent peer (header, payload) = do
	putStrLn $ "Received " ++ (show header)
	case header of
		MsgChoke -> writeIORef (prChoking peer) True
		MsgUnchoke -> writeIORef (prChoking peer) False
		MsgInterested -> writeIORef (prInterested peer) True
		MsgUninterested -> writeIORef (prInterested peer) False
		MsgHave -> do
			let idx = (readInt $ head payload)
			writeArray (prPieces peer) idx True

			piece <- readArray (pieces torrent) idx
			isComplete <- readIORef (pcComplete piece)
			isInteresting <- readIORef (prInteresting peer)
			if (isComplete == False) && (isInteresting == False)
				then do
					writeIORef (prInteresting peer) True
					sendMessage (prHandle peer) MsgInterested []
					return ()
				else return ()
		MsgBitfield -> do

			-- Parse the payload into a list of Bools, then make an assocs call.

			return ()
		MsgRequest -> do
			let [pieceIdx, offset, len] = payload

			-- Check if I have the requested block of the requested piece.  If so, send a MsgPiece message.

			return ()
		MsgPiece -> do
			let [pieceIdx, offset, content] = payload

			piece <- readArray (pieces torrent) (readInt pieceIdx)
			writeBlock piece (readInt offset) content

			return ()
		MsgCancel -> do
			let [pieceIdx, offset, len] = payload

			-- Make sure to never send the peer the specified block.

			return ()

addInactivePeer :: InactivePeer -> IORef [InactivePeer] -> IO ()
addInactivePeer p ps = modifyIORef ps (p:)

addActivePeer :: ActivePeer -> IORef [ActivePeer] -> IO ()
addActivePeer p ps = modifyIORef ps (p:)

torrentFromFile :: String -> IO Torrent
torrentFromFile filename = do
	m <- M.metainfoFromFile filename
	torrentFromMetainfo m

torrentFromURL :: String -> IO Torrent
torrentFromURL url = do
	m <- M.metainfoFromURL url
	torrentFromMetainfo m

torrentFromMetainfo :: M.Metainfo -> IO Torrent
torrentFromMetainfo m = do
	inactives <- newIORef []
	actives <- newIORef []
	pieces <- mapM (\x -> mkPiece x (M.infoPieceLength m) (M.infoPieceHash m x)) [1..(M.infoPieceCount m)]
	pieceArray <- newListArray (0, (M.infoPieceCount m) - 1) pieces
	return Torrent {
		metainfo = m,
		announcer = defaultAnnouncer m,
		inactivePeers = inactives,
		activePeers = actives,
		pieces = pieceArray
	}

defaultAnnouncer :: M.Metainfo -> Announcer
defaultAnnouncer m = Announcer {
	url = M.announce m,
	infoHash = M.infoHash m,
	uploaded = 0,
	downloaded = 0,
	left = M.infoLength m
}
