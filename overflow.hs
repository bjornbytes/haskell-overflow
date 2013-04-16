module Main where

import BEncode
import Metainfo
import Data.Maybe

main :: IO ()
main = do
  m <- metainfoFromFile "torrent.torrent"
  print m