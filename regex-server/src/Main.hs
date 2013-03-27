{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Regex
--import System.IO.Temp (withTempFile)
import Data.GraphViz.Commands (graphvizWithHandle, GraphvizCommand(Dot), GraphvizOutput(Svg))
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8 (unpack)

main :: IO ()
main =  quickHttpServe site

render :: ByteString -> IO ByteString
render input = let r = compile (unpack input)
                   dot = toDot r
               in graphvizWithHandle Dot dot Svg hGetContents

site :: Snap ()
site =
    ifTop (writeBS "Hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("regex", serveImage)
          ] <|>
    dir "static" (serveDirectory "static")

serveImage :: Snap ()
serveImage = do Just [rawRegex] <- getsRequest $ rqQueryParam "r"
                image <- liftIO $ render rawRegex
                writeBS image
                modifyResponse $ setContentType svgContentType
                modifyResponse $ setResponseCode 200
  where svgContentType = "image/svg+xml"                  

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
