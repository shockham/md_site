{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe
import Network.Wai.Middleware.Static

import Data.Monoid
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Control.Monad  
import Control.Monad.Trans (liftIO)

import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


main :: IO ()
main =
    runSpock 8080 $ spockT id $ do
        middleware $ staticPolicy (addBase "static")

        get root $ do
            content <- liftIO $ fromMdFile "md/index.md" 
            html content
        get var $ \name -> do
            content <- liftIO $ fromMdFile ("md/" <> name <> ".md")
            html content

fromMdFile :: FilePath -> IO T.Text
fromMdFile f = liftM renderMd $ readFile f

renderMd :: String -> T.Text
renderMd md = toStrict $ renderHtml $ mdPage md

mdPage :: String -> H.Html
mdPage md = H.html $ do
    H.head $ do
        H.title "md_site."
    H.body $ do
        markdown def $ L.pack md
