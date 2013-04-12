module Main where

import BEncode
import Metainfo
import Data.Maybe

main :: IO ()
main = do
  print $ decodeBString "4:spam"