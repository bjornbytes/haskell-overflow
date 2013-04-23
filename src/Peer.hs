module Peer where

import qualified Data.ByteString as B
import System.IO
import Data.IORef
import Data.Array.IO

data InactivePeer = InactivePeer {
	ip :: String,
	port :: Int
} deriving (Show)

data ActivePeer = ActivePeer {
	prId :: B.ByteString,
	prHandle :: Handle,

	prInterested :: IORef Bool,     --The peer is interested in us.
	prInteresting :: IORef Bool,    --The peer is interesting to us.
	prChoked :: IORef Bool,         --The peer is choked by us.
	prChoking :: IORef Bool,        --The peer is choking us.

	prPieces :: IOUArray Int Bool   --Maps piece indices (0-indexed) to a Bool indicating whether or not the peer has the piece (not always 100% accurate).
}
