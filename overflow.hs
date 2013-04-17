module Main where

import Metainfo

main :: IO ()
main = do
  m <- metainfoFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
  print $ announce m
