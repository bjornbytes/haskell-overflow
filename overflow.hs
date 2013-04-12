module Main where

import BEncode
import Metainfo
import Data.Maybe

main :: IO ()
main = do
  print (decodeBList "l4:spam4:eggse")