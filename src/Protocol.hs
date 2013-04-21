module Protocol where

import Control.Monad
import Data.Maybe
import Network
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Config
import Peer

data MsgHeader = MsgKeepAlive | MsgChoke | MsgUnchoke | MsgInterested | MsgUninterested | MsgHave | MsgBitfield | MsgRequest | MsgPiece | MsgCancel
                 deriving (Enum)


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
              -> IO ActivePeer -- The peer constructed from the received handshake.
recvHandshake handle = do
	str <- B.hGetLine handle
	let [pLen, pStr, rsrv, hash, peer] = slices [0, 1, 20, 28, 48, 68] str
	case (C.unpack pStr) == protocol of
		True  -> return ActivePeer {
                          peerId = peer,
                          prHandle = handle,
                          interested = False,
                          interesting = False,
                          choked = True,
                          choking = True
	                    }
		False -> error $ "Connected client is not using \"" ++ protocol ++ "\" protocol."


recvMessage :: Handle
            -> IO (MsgHeader, [B.ByteString])
recvMessage handle = do
	size <- B.hGet handle 4
	case size of
		0 -> return (MsgKeepAlive, [B.empty])
		otherwise -> do
			body <- B.hGet handle size
			return parseMessage $ slices [0, 1, size-1] body


parseMessage :: [B.ByteString]              -- Id and payload of message
             -> (MsgHeader, [B.ByteString])
parseMessage [msgId, payload] = case msgId of
	0 -> (MsgChoke, [B.empty])
	1 -> (MsgUnchoke, [B.empty])
	2 -> (MsgInterested, [B.empty])
	3 -> (MsgUninterested, [B.empty])
	4 -> (MsgHave, [payload])
	5 -> (MsgBitfield, [payload])
	6 -> (MsgRequest, [B.empty])
	7 -> (MsgPiece, [B.empty])
	8 -> (MsgCancel, [B.empty])
	otherwise -> error $ "Unknown message id \"" ++ (show msgId) ++ "\"."

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start len str = B.take len $ snd $ B.splitAt start str


slices :: [Int]          -- A list of slicing points (should include 0 and the index of the last character)
       -> B.ByteString   -- The bytestring to slice
       -> [B.ByteString] -- A list of bytestrings, such that the nth element is a range of characters from the nth to n+1'th elements of the slicing points.
slices pivots string = map (\(x,y) -> slice x (y-x) string) $ zip pivots $ tail pivots