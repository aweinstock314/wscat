{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets as WS

lGetLine = fmap (L.pack . map (fromIntegral . ord)) getLine
safeRead = fmap fst . listToMaybe . reads

-- Gracefully close the connection and mask the default printout
exnHandler :: WS.Connection -> SomeException -> IO ()
exnHandler conn _ = WS.sendClose conn L.empty

server = WS.acceptRequest >=> client

client conn = (handle $ exnHandler conn) $ do
    forkIO . forever $ lGetLine >>= WS.sendBinaryData conn
    forever $ WS.receiveData conn >>= L.putStr

data WSCatConfig = WSCatConfig {
    cfgHostname :: Maybe String,
    cfgPort :: Maybe Int,
    cfgPath :: Maybe String,
    cfgMainFunc :: WSCatConfig -> IO ()
    }

defaultConfig = WSCatConfig { cfgHostname = Nothing, cfgPort = Nothing, cfgPath = Nothing, cfgMainFunc = clientMain }

getOptOptions = [
    Option "l" [] (NoArg $ \cfg -> return cfg {cfgMainFunc = serverMain}) "Listen for inbound websocket connections.",
    Option "p" [] (ReqArg (\arg cfg -> return cfg {cfgPort = safeRead arg}) "PORT") "Which port number to use."
    ]

applyLeftovers cfg [] = cfg
applyLeftovers cfg [port] = cfg { cfgPort = safeRead port }
applyLeftovers cfg [hostname, port] = applyLeftovers (cfg { cfgHostname = Just hostname }) [port]
applyLeftovers cfg [hostname, port, path] = applyLeftovers (cfg { cfgPath = Just path }) [hostname, port]

errorHelp :: a
errorHelp = error $ usageInfo (unlines ["","\twscat -l [-p] PORT","\twscat HOST PORT [PATH]"]) getOptOptions

main = do
    (cfg, leftovers, errors) <- fmap (getOpt RequireOrder getOptOptions) getArgs
    unless (null errors) . error $ unlines errors
    cfg' <- foldl (>>=) (return defaultConfig :: IO WSCatConfig) cfg
    let cfg'' = applyLeftovers cfg' leftovers
    cfgMainFunc cfg'' $ cfg''

clientMain (WSCatConfig {cfgHostname = hostname, cfgPort = port, cfgPath = path}) =
    WS.runClient hostname' port' path' client where
        hostname' = fromMaybe errorHelp hostname
        port' = fromMaybe errorHelp port
        path' = fromMaybe "/" path

serverMain (WSCatConfig {cfgPort = port}) =
    WS.runServer "0.0.0.0" port' server where
        port' = fromMaybe errorHelp port
