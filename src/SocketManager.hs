module SocketManager where

import Network
import System.IO
import Control.Concurrent

import TorrentManager

listenOn :: Int -> MVar () -> IO ()
listenOn port done = do
  sock <- Network.listenOn $ PortNumber $ fromIntegral port
  putStrLn $ "Listening on " ++ (show port) ++ "..."
  acceptHandler sock done

acceptHandler :: Socket -> MVar () -> IO ()
acceptHandler sock done = do
  credentials@(handle, ip, port) <- accept sock
  putStrLn $ "New connection from " ++ ip ++ ":" ++ (show port)
  hSetBuffering handle NoBuffering
  forkIO $ handshakeHandler handle
  acceptHandler sock done
  putMVar done ()