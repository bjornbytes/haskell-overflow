module Main where

import Control.Concurrent
import Control.Monad

import Config
import Torrent
import TorrentManager
import SocketManager

main :: IO ()
main = do
	done <- newEmptyMVar
	forkIO $ listenOn localPort done
	t <- addTorrentFromURL "http://cdimage.ubuntu.com/kubuntu/releases/quantal/release/kubuntu-12.10-desktop-amd64.iso.torrent"
	start t
	takeMVar done