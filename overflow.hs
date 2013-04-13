module Main where

import BEncode
import Metainfo
import Data.Maybe

main :: IO ()
main = do
  metainfoFromFile "torrent.torrent"