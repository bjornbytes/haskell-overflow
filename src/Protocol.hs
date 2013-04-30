module Protocol where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Network
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary.Put
import Data.Binary.Get
import Data.IORef
import Data.Array.IO

import Config
import Peer
import Metainfo

data MsgHeader = MsgKeepAlive | MsgChoke | MsgUnchoke | MsgInterested | MsgUninterested | MsgHave | MsgBitfield | MsgRequest | MsgPiece | MsgCancel
                 deriving (Show, Enum)


sendHandshake :: Handle       -- The handle to write to.
              -> B.ByteString -- The infoHash to send.
              -> B.ByteString -- The peerId to send.
              -> IO ()
sendHandshake handle hash peer = C.hPutStr handle hMsg
	where hMsg = B.concat [pLen, pStr, rsrv, hash, peer]
	      pLen = B.singleton (fromIntegral $ length protocol)
	      pStr = C.pack protocol
	      rsrv = B.replicate 8 (fromIntegral 0)


recvHandshake :: Handle        -- The handle to receive from.
              -> Metainfo      -- The metainfo object associated with this torrent.
              -> IO ActivePeer -- The peer constructed from the received handshake.
recvHandshake handle metainfo = do
	pLen <- B.hGet handle 1
	pStr <- B.hGet handle (readWord pLen)
	rsrv <- B.hGet handle 8
	hash <- B.hGet handle 20
	peer <- B.hGet handle 20
	case (C.unpack pStr) == protocol of
		True  -> do
			interested <- newIORef False
			interesting <- newIORef False
			choked <- newIORef True
			choking <- newIORef False
			pieces <- newArray (0, (infoPieceCount metainfo) - 1) False
			wanted <- newIORef (-1)
			return ActivePeer {
                          prId = peer,
                          prHandle = handle,
                          prInterested = interested,
                          prInteresting = interesting,
                          prChoked = choked,
                          prChoking = choking,
                          prPieces = pieces,
                          prWantedPiece = wanted
                        }
		False -> error $ "Connected client is not using \"" ++ protocol ++ "\" protocol."


recvMessage :: Handle -> IO (MsgHeader, [B.ByteString])
recvMessage handle = do
	sizeBytes <- B.hGet handle 4
	if (B.length sizeBytes) < 4
		then do
			threadDelay 1000000 -- For some reason, on some handles I constantly read 0 bytes even though I request 4 (it should block until it gets 4 -- maybe it's an EOF?).
			                    -- So to prevent an infinite loop of 0-reads on these handles, which eats CPU, I just wait for 1 second before reading again.
			recvMessage handle
		else do
			let size = readInt sizeBytes
			case size of
				0 -> return (MsgKeepAlive, [B.empty])
				otherwise -> do
					body <- B.hGet handle size
					return $ parseMessage $ slices [0, 1, size] body

sendMessage :: Handle
            -> MsgHeader
            -> [B.ByteString]
            -> IO ()
sendMessage handle header payload = do
	putStrLn $ "Sending " ++ (show header)
	case header of
		MsgKeepAlive -> B.hPut handle $ writeInt 0
		MsgChoke -> B.hPut handle $ B.concat [writeInt 1, writeWord 0]
		MsgUnchoke -> B.hPut handle $ B.concat [writeInt 1, writeWord 1]
		MsgInterested -> B.hPut handle $ B.concat [writeInt 1, writeWord 2]
		MsgUninterested -> B.hPut handle $ B.concat [writeInt 1, writeWord 3]
		MsgHave -> B.hPut handle $ B.append (B.concat [writeInt 5, writeWord 4]) (B.concat payload)
		--MsgBitfield ->
		MsgRequest -> B.hPut handle $ B.append (B.concat [writeInt 13, writeWord 6]) (B.concat payload)
		MsgPiece -> B.hPut handle $ B.append (B.concat [writeInt (9 + (B.length (last payload))), writeWord 7]) (B.concat payload)
		otherwise ->  return ()

parseMessage :: [B.ByteString]              -- Id and payload of message
             -> (MsgHeader, [B.ByteString])
parseMessage [msgId, payload] = case (fromIntegral $ B.head msgId) of
	0 -> (MsgChoke, [B.empty])
	1 -> (MsgUnchoke, [B.empty])
	2 -> (MsgInterested, [B.empty])
	3 -> (MsgUninterested, [B.empty])
	4 -> (MsgHave, [payload])
	5 -> (MsgBitfield, [payload])
	6 -> (MsgRequest, slices [0, 4, 8, 12] payload)
	7 -> (MsgPiece, slices [0, 4, 8, (B.length payload)] payload)
	8 -> (MsgCancel, slices [0, 4, 8, 12] payload)
	otherwise -> error $ "Unknown message id \"" ++ (show msgId) ++ "\"."

readInt :: B.ByteString -> Int
readInt x = fromIntegral $ runGet getWord32be $ L.fromChunks . return $ x

readWord :: B.ByteString -> Int
readWord x = fromIntegral $ runGet getWord8 $ L.fromChunks . return $ x

writeInt :: Int -> B.ByteString
writeInt x = B.concat . L.toChunks $ runPut $ putWord32be $ fromIntegral $ x

writeWord :: Int -> B.ByteString
writeWord x = B.singleton (fromIntegral x)

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start len str = B.take len $ snd $ B.splitAt start str


slices :: [Int]          -- A list of slicing points (should include 0 and the index of the last character)
       -> B.ByteString   -- The bytestring to slice
       -> [B.ByteString] -- A list of bytestrings, such that the nth element is a range of characters from the nth to n+1'th elements of the slicing points.
slices pivots string = map (\(x,y) -> slice x (y-x) string) $ zip pivots $ tail pivots