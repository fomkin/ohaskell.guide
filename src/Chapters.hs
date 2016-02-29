{-# LANGUAGE OverloadedStrings #-}

module Chapters (
      prepareTemplates
    , createCoverPage
    , createInitPage
    , createChapters
) where

import Hakyll

prepareTemplates :: Rules ()
prepareTemplates = match "templates/*" $ compile templateCompiler

createCoverPage :: Rules ()
createCoverPage = create ["index.html"] $ do
    route idRoute
    compile $
        let indexContext = mconcat [ constField "title" "#ohaskell"
                                   , defaultContext
                                   ]
        in makeItem "" >>= loadAndApplyTemplate "templates/cover.html" indexContext
                       >>= relativizeUrls

createInitPage :: Rules ()
createInitPage = match markdownPage $ do
    route $ removeChaptersDirectoryFromURLs `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= loadAndApplyTemplate templateName defaultContext
                             >>= relativizeUrls
  where
    markdownPage = fromGlob $ "chapters/init.md"
    templateName = fromFilePath $ "templates/default.html"

createChapters :: Rules ()
createChapters = match chapters $ do
    route $ removeChaptersDirectoryFromURLs `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= loadAndApplyTemplate chapterTemplateName defaultContext
                             >>= loadAndApplyTemplate defaulTemplateName defaultContext
                             >>= relativizeUrls
  where
    chapters            = fromGlob $ "chapters/**"
    chapterTemplateName = fromFilePath $ "templates/chapter.html"
    defaulTemplateName  = fromFilePath $ "templates/default.html"

removeChaptersDirectoryFromURLs :: Routes
removeChaptersDirectoryFromURLs = gsubRoute "chapters/" (const "")

