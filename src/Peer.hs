module Peer where

import qualified Data.ByteString as B;
import Network.Socket;

data Peer = Peer {
  ip :: String,
  port :: Int,
  
  interested :: Bool,  --The peer is interested in us.
  interesting :: Bool, --The peer is interesting to us.
  choked :: Bool,      --The peer is choked by us.
  choking :: Bool      --The peer is choking us.
} deriving (Show)
