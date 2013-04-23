module Piece where

import qualified Data.ByteString as B
import Data.Array.IO
import Data.IORef

data Piece = Piece {
	pcComplete :: IORef Bool,
	pcIndex :: Int,
	pcSize :: Int,
	pcHash :: B.ByteString,

	pcBlocks :: IOArray Int Bool -- Maps block indices to Blocks (bytestrings).  It is assumed that Blocks are 16K long.  If a block is received that is larger,
	                             -- then it is split into 16K chunks.
}

mkEmptyBlockArray :: Int -> IO (IOArray Int Bool)
mkEmptyBlockArray size = newArray (0, size-1) False

mkPiece :: Int -> Int -> B.ByteString -> IO Piece
mkPiece idx size hash = do
	complete <- newIORef False
	blocks <- mkEmptyBlockArray (size `div` 16384)
	return Piece {
		pcComplete = complete,
		pcIndex = idx,
		pcSize = size,
		pcHash = hash,
		pcBlocks = blocks
	}