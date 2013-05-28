{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Snap.Core
import Snap.Http.Server
import Regex
import Data.GraphViz.Types (setID, toGraphID)
import Data.GraphViz.Commands (graphvizWithHandle, GraphvizCommand(Dot), GraphvizOutput(Svg))
import Data.ByteString hiding (unpack)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (fromMaybe)

main :: IO ()
main =  quickHttpServe site

site :: Snap ()
site = route [ ("regex", serveImage)
             ]

serveImage :: Snap ()
serveImage = do (raw:_) <- fmap (fromMaybe [""]) $ getsRequest $ rqQueryParam "r"
                let decoded = UTF8.toString raw
                case compile decoded of
                  (Left err)    -> do
                    modifyResponse $ setContentType plaintextContentType
                    modifyResponse $ setResponseCode 400
                    writeBS $ UTF8.fromString err
                  (Right regex) -> do
                    image <- liftIO $ render regex decoded
                    modifyResponse $ setContentType svgContentType
                    modifyResponse $ setResponseCode 200
                    writeBS image
  where svgContentType = "image/svg+xml"
        plaintextContentType = "text/plain"

render :: Regex -> String -> IO ByteString
render regex name = let dot = toDot regex
                        namedDot = setID (toGraphID name) dot
                    in graphvizWithHandle Dot namedDot Svg hGetContents
