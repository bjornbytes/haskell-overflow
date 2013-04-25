module Piece where

import qualified Data.ByteString as B
import Data.Array.IO
import Data.IORef

data Piece = Piece {
	pcComplete :: IORef Bool,
	pcIndex :: Int,
	pcSize :: Int,
	pcHash :: B.ByteString,

	pcBlocks :: IOArray Int B.ByteString, -- Maps block indices to Blocks (bytestrings).  It is assumed that Blocks are 16K long.  If a block is received that is larger,
	                                      -- then it is split into 16K chunks.
	pcBitfield :: IOUArray Int Bool
}

mkEmptyBlockArray :: Int -> IO (IOArray Int B.ByteString)
mkEmptyBlockArray size = newArray (0, size-1) B.empty

writeBlock :: Piece -> Int -> B.ByteString -> IO ()
writeBlock piece offset content = do
	writeArray (pcBlocks piece) (offset `div` 16384) content
	writeArray (pcBitfield piece) (offset `div` 16384) True
	bitfield <- getElems $ (pcBitfield piece)
	case or $ bitfield of
		True -> do
			writeIORef (pcComplete piece) True
			return ()
		False -> return ()

mkPiece :: Int -> Int -> B.ByteString -> IO Piece
mkPiece idx size hash = do
	complete <- newIORef False
	blocks <- mkEmptyBlockArray (size `div` 16384)
	bitfield <- newArray (0, (size `div` 16384) - 1) False
	return Piece {
		pcComplete = complete,
		pcIndex = idx,
		pcSize = size,
		pcHash = hash,
		pcBlocks = blocks,
		pcBitfield = bitfield
	}