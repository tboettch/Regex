{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Snap.Core
import Snap.Http.Server
import Regex
--import System.IO.Temp (withTempFile)
import Data.GraphViz.Types (setID, toGraphID)
import Data.GraphViz.Commands (graphvizWithHandle, GraphvizCommand(Dot), GraphvizOutput(Svg))
import Data.ByteString hiding (unpack)
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main =  quickHttpServe site

site :: Snap ()
site = route [ ("regex", serveImage)
             ]

serveImage :: Snap ()
serveImage = do Just [rawRegex] <- getsRequest $ rqQueryParam "r"
                image <- liftIO $ render rawRegex
                writeBS image
                modifyResponse $ setContentType svgContentType
                modifyResponse $ setResponseCode 200
  where svgContentType = "image/svg+xml"

render :: ByteString -> IO ByteString
render input = let text = UTF8.toString input
                   r = compile text
                   dot = toDot r
                   namedDot = setID (toGraphID text) dot
               in graphvizWithHandle Dot namedDot Svg hGetContents
