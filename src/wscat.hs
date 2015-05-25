{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Concurrent
import Control.Monad
import Data.Char
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets as WS

lGetLine = fmap (L.pack . map (fromIntegral . ord)) getLine

server = WS.acceptRequest >=> client

client conn = do
    forkIO . forever $ lGetLine >>= WS.sendBinaryData conn
    forever $ WS.receiveData conn >>= L.putStr

-- TODO: consider more robust argument parsing
main = getArgs >>= \case
    ["-l", "-p", port] -> WS.runServer "0.0.0.0" (read port) server
    [hostname, port, path] -> WS.runClient hostname (read port) path client
