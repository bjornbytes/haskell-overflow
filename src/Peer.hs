module Peer where

import qualified Data.ByteString as B;
import System.IO
import Network;

data InactivePeer = InactivePeer {
  ip :: String,
  port :: Int
} deriving (Show)

data ActivePeer = ActivePeer {
  peerId :: B.ByteString,
  prHandle :: Handle,

  interested :: Bool,  --The peer is interested in us.
  interesting :: Bool, --The peer is interesting to us.
  choked :: Bool,      --The peer is choked by us.
  choking :: Bool      --The peer is choking us.
}
