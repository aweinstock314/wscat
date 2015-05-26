{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Char
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets as WS

lGetLine = fmap (L.pack . map (fromIntegral . ord)) getLine

-- Gracefully close the connection and mask the default printout
exnHandler :: WS.Connection -> SomeException -> IO ()
exnHandler conn _ = WS.sendClose conn L.empty

server = WS.acceptRequest >=> client

client conn = (handle $ exnHandler conn) $ do
    forkIO . forever $ lGetLine >>= WS.sendBinaryData conn
    forever $ WS.receiveData conn >>= L.putStr

-- TODO: consider more robust argument parsing
main = getArgs >>= \case
    ["-l", "-p", port] -> WS.runServer "0.0.0.0" (read port) server
    [hostname, port, path] -> WS.runClient hostname (read port) path client
