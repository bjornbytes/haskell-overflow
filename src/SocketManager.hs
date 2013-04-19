module SocketManager where

import Network

socketVoid :: [Socket]
socketVoid = []

listenOn :: Int -> IO ()
listenOn port = do
  sock <- Network.listenOn $ PortNumber $ fromIntegral port
  putStrLn $ "Listening on " ++ (show port) ++ "..."
  acceptHandler sock

acceptHandler :: Socket -> IO ()
acceptHandler sock = do
  (handle, ip, port) <- accept sock
  putStrLn $ "New connection from " ++ (show ip) ++ ":" ++ (show port)
  --Do stuff
  acceptHandler sock
