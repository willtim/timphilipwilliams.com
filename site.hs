{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.Monoid
import System.FilePath
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Pandoc
import Hakyll
import Hakyll.Images (loadImage, scaleImageCompiler)

config :: Configuration
config = defaultConfiguration

feedConfiguration = FeedConfiguration
    { feedTitle       = "timphilipwilliams.com"
    , feedDescription = "A functional programming blog."
    , feedAuthorName  = "Tim Williams"
    , feedAuthorEmail = "info@timphilipwilliams.com"
    , feedRoot        = "http://www.timphilipwilliams.com"
    }

main :: IO ()
main = hakyllWith config $ do

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each post
    match "posts/*" $ do
      route $ setExtension ".html"
      compile $ myPandocCompiler
            >>= applyTemplates ["post", "default", "scaffold"] myDefaultCtx
            >>= relativizeUrls

    -- site index - currently a list of all my posts
    create ["index.html"] $ do
      route idRoute
      compile $ do
        items   <- recentFirst =<< loadAll "posts/*"
        itemTpl <- loadBody "templates/postitem.html"
        list    <- applyTemplateList itemTpl myDefaultCtx items
        let ctx =  constField "title" "Posts"
                <> constField "postsurl" nullLink
                <> constField "postsclass" "active"
                <> constField "posts" list
                <> myDefaultCtx
        makeItem ""
          >>= applyTemplates ["posts", "default", "scaffold"] ctx
          >>= relativizeUrls

    match "slides.html" $ do
        route   idRoute
        let ctx =  constField "title" "Slides"
                <> constField "slidesurl" nullLink
                <> constField "slidesclass" "active"
                <> myDefaultCtx
        compile $ getResourceBody
              >>= applyTemplates ["default", "scaffold"] ctx
              >>= relativizeUrls

    match "projects.html" $ do
        route   idRoute
        let ctx =  constField "title" "Projects"
                <> constField "projectsurl" nullLink
                <> constField "projectsclass" "active"
                <> myDefaultCtx
        compile $ getResourceBody
              >>= applyTemplates ["default", "scaffold"] ctx
              >>= relativizeUrls

    match "aboutme.html" $ do
        route   idRoute
        let ctx =  constField "title" "About me"
                <> constField "aboutmeurl" nullLink
                <> constField "aboutmeclass" "active"
                <> myDefaultCtx
        compile $ getResourceBody
              >>= applyTemplates ["default", "scaffold"] ctx
              >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        route tagRoute
        compile $ do
            items   <- recentFirst =<< loadAll pattern
            itemTpl <- loadBody "templates/postitem.html"
            list    <- applyTemplateList itemTpl myDefaultCtx items
            let ctx =  constField "tag"   tag
                    <> constField "posts" list
                    <> myDefaultCtx
            makeItem ""
              >>= applyTemplates ["postswithtag", "default", "scaffold"] ctx
              >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            cloud <- renderTagCloud 150 300 tags
            let ctx =  constField "title" "Tags"
                    <> constField "tagsurl" nullLink
                    <> constField "tagsclass" "active"
                    <> constField "tags" cloud
                    <> myDefaultCtx
            makeItem ""
              >>= applyTemplates ["tags", "default", "scaffold"] ctx
              >>= relativizeUrls

    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*.js" $ do
        route   idRoute
        compile copyFileCompiler

    -- for the Minecraft images
    match "img/minecraft/**.png" $ do
        route idRoute
        compile $ loadImage
           >>= scaleImageCompiler 1024 768

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "slides/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["rss.xml"] $ do
      route idRoute
      compile $ loadAll "posts/*"
        >>= renderRss feedConfiguration myDefaultCtx

    -- 404
    -- Don't relativize URLs
    match "static/404.html" $ do
        route   $ constRoute "404.html"
        let ctx =  constField "postsclass" ""
                <> constField "title" "404"
                <> constField "robots" "noindex, nofollow, noarchive, nocache"
                <> myDefaultCtx
        compile $ getResourceBody
            >>= applyTemplates ["default", "scaffold"] ctx

-- | Consistent convention for links that don't go anywhere
nullLink :: String
nullLink = "javascript:void(0)"

-- | Allow for reference style links in markdown
--   and disable the leading >
pandocWriteOptions :: WriterOptions
pandocWriteOptions = defaultHakyllWriterOptions
    {  writerExtensions     = disableExtension Ext_literate_haskell
                                (writerExtensions defaultHakyllWriterOptions)
     , writerReferenceLinks = True
    }

pandocReadOptions :: ReaderOptions
pandocReadOptions = defaultHakyllReaderOptions
    { readerExtensions = disableExtension Ext_markdown_in_html_blocks
                            (readerExtensions defaultHakyllReaderOptions)
    }

myPandocCompiler = pandocCompilerWith pandocReadOptions pandocWriteOptions

-- | Default setup is for individual post pages
myDefaultCtx :: Context String
myDefaultCtx = mconcat [
        constField "robots" "index, follow"
      , constField "homeclass" "active"
      , constField "homeurl" "/index.html"
      , constField "postsclass"  ""
      , constField "slidesclass" ""
      , constField "projectsclass" ""
      , constField "aboutmeclass"  ""
      , constField "tagsclass"  ""
      , constField "postsurl"  "/"
      , constField "slidesurl"  "/slides.html"
      , constField "projectsurl"  "/projects.html"
      , constField "aboutmeurl"  "/aboutme.html"
      , constField "tagsurl"  "/tags.html"
      , constField "author" "Tim Williams"
      , dateField "date" "%e %B %Y"
      , defaultContext
      ]

applyTemplates :: [String] -> Context String -> Item String -> Compiler (Item String)
applyTemplates names ctx = foldr apply return names
    where
      apply name = (loadAndApplyTemplate (templ name) ctx >=>)
      templ name = fromFilePath $ "templates/" ++ name ++ ".html"

tagRoute :: Routes
tagRoute = foldl1 composeRoutes
    [ setExtension ".html"
    , gsubRoute "." adjustLink
    , gsubRoute "/" (const "")
    , gsubRoute "^tag" (const "tag/")
    , gsubRoute "-html" (const "/index.html") ]

adjustLink = filter (not . isSlash) . map (toLower . replaceWithDash)
  where
    replaceWithDash c | c == '.' || c == ' ' = '-'
                      | otherwise = c
    isSlash '/' = True
    isSlash _   = False
