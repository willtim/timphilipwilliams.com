{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mappend, mempty, mconcat)
import Data.Char (toLower)
import qualified Data.Map as M

import Text.Pandoc
import Hakyll

config = defaultHakyllConfiguration

feedConfiguration = FeedConfiguration
    { feedTitle       = "timphilipwilliams.com"
    , feedDescription = "A functional programming blog."
    , feedAuthorName  = "Tim Williams"
    , feedAuthorEmail = "info@timphilipwilliams.com"
    , feedRoot        = "http://timphilipwilliams.com"
    }

main :: IO ()
main = hakyllWith config $ do
              
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render each post
    match "posts/*" $ do
      route $ setExtension ".html"
      compile $ myPageCompiler
            >>> applyTemplateCompilers ["post", "default", "scaffold"]
            >>> relativizeUrlsCompiler

    -- site index - currently a list of all my posts
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
      >>> addMyDefaultFields
      >>> arr (setField "title" "Posts")
      >>> arr (setField "postsurl" nullLink)
      >>> arr (setField "postsclass" "active")
      >>> setFieldPageList recentFirst "templates/postitem.html" "posts" "posts/*"
      >>> applyTemplateCompilers ["posts", "default", "scaffold"]
      >>> relativizeUrlsCompiler    
    
    match "aboutme.html" $ do
        route   $ idRoute
        compile $ readPageCompiler
            >>> addMyDefaultFields
            >>> arr (setField "title" "About me")
            >>> arr (setField "aboutmeurl" nullLink)
            >>> arr (setField "aboutmeclass" "active")
            >>> applyTemplateCompilers ["default", "scaffold"]
            >>> relativizeUrlsCompiler

    match "tags.html" $ do
        route   $ idRoute
        compile $ readPageCompiler
            >>> addMyDefaultFields
            >>> arr (setField "title" "Tags")
            >>> arr (setField "tagsurl" nullLink)
            >>> arr (setField "tagsclass" "active")
            >>> applyTemplateCompilers ["default", "scaffold"]
            >>> relativizeUrlsCompiler

    -- Copy fonts
    match "font/*" $ do
        route   $ idRoute
        compile $ copyFileCompiler

    -- Copy CSS
    match "css/*.css" $ do
        route   $ idRoute
        compile $ compressCssCompiler

    -- Copy Javascript
    match "js/*.js" $ do
        route   $ idRoute
        compile $ copyFileCompiler

    -- Copy images
    match "img/*" $ do
        route   $ idRoute
        compile $ copyFileCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tag/*" $ route $ tagRoute
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier "tag/*" t, 
                                  makePostList "Posts with the tag" t p)))

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- 404
    -- Don't relativize URLs
    match "static/404.shtml" $ do
        route   $ constRoute "errors/404.shtml"
        compile $ readPageCompiler
            >>> addMyDefaultFields
            >>> arr (setField "postsclass" "")
            >>> arr (setField "title" "404")
            >>> arr (setField "robots" "noindex, nofollow, noarchive, nocache")
            >>> applyTemplateCompilers ["default", "scaffold"]
 
    -- 403
    -- Don't relativize URLs
    match "static/403.shtml" $ do
        route   $ constRoute "errors/403.shtml"
        compile $ readPageCompiler
            >>> addMyDefaultFields
            >>> arr (setField "postsclass" "")
            >>> arr (setField "title" "403")
            >>> arr (setField "robots" "noindex, nofollow, noarchive, nocache")
            >>> applyTemplateCompilers ["default", "scaffold"]


-- | Consistent convention for links that don't go anywhere
nullLink :: String
nullLink = "javascript:void(0)"

-- | Default setup is for individual post pages
addMyDefaultFields = arr (trySetField "robots" "index, follow")
    >>> arr (trySetField "postsclass" "")
    >>> arr (trySetField "aboutmeclass" "")
    >>> arr (trySetField "tagsclass" "")
    >>> arr (trySetField "postsurl" "/")
    >>> arr (trySetField "aboutmeurl" "/aboutme.html")
    >>> arr (trySetField "tagsurl" "/tags.html")
    >>> arr (trySetField "author" "Tim Williams")
    >>> arr (renderDateField "date" "%e %B %Y" "")

-- | Allow for reference style links in markdown
--   and disable the leading >
pandocWriteOptions = defaultWriterOptions {
      writerReferenceLinks = True
    , writerLiterateHaskell = False
    }

myPageCompiler :: Compiler Resource (Page String)
myPageCompiler = readPageCompiler
    >>> addDefaultFields
    >>> addMyDefaultFields
    >>> arr applySelf
    >>> pageRenderPandocWith defaultHakyllParserState pandocWriteOptions

applyTemplateCompilers :: [String] -> Compiler (Page String) (Page String)
applyTemplateCompilers [] = arr id
applyTemplateCompilers (x:xs) = applyTemplateCompiler templ >>> applyTemplateCompilers xs
    where templ = parseIdentifier ("templates/" ++ x ++ ".html")

tagIdentifier :: Pattern (Page String) -> String -> Identifier (Page String)
tagIdentifier pattern = fromCapture pattern

-- | Auxiliary compiler: generate a post list from a list of given posts, and
--   add it to the current page under @$posts@
addPostList :: String -> Identifier (Template) -> Compiler (Page String, [Page String]) (Page String)
addPostList field template = setFieldA field $
    arr recentFirst
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
        
makePostList :: String
            -> String
            -> [Page String]
            -> Compiler () (Page String)
makePostList message tag posts =
    constA (mempty, posts)
        >>> addPostList "posts" "templates/postitem.html"
        >>> arr (setField "title" (message ++ " &#8216;" ++ tag ++ "&#8217;"))
      --  >>> arr (setField "tags" tag)
      --  >>> arr (setField "route" (adjustLink tag))
        >>> applyTemplateCompilers ["posts", "default", "scaffold"]
        >>> relativizeUrlsCompiler

-- TODO clean up
tagRoute :: Routes
tagRoute =
    setExtension ".html" `composeRoutes`
    gsubRoute "." adjustLink `composeRoutes`
        gsubRoute "/" (const "") `composeRoutes`
            gsubRoute "^tag" (const "tag/") `composeRoutes`
                gsubRoute "-html" (const "/index.html")

adjustLink = filter (not . isSlash) . map (toLower . replaceWithDash)
  where
    replaceWithDash c | c == '.' || c == ' ' = '-'
                      | otherwise = c
    isSlash '/' = True
    isSlash _   = False
