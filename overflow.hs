module Main where

import BEncode

main :: IO ()
main = do
  let x = (BList [(BInteger 4), (BInteger 6)])
  print x
  decode "d4:datal4:spam4:eggse5:bjorn7:swensone"