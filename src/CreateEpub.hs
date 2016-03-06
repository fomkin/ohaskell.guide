{-# LANGUAGE OverloadedStrings #-}

module CreateEpub (
    createEpub
) where

import System.Process (callCommand)

import CreateEpubCss

createEpub :: FilePath -> IO ()
createEpub pathToSingleMarkdown = do
    createEpubCss pathToCss
    callCommand $ concat [ "pandoc -S -o "
                         , out
                         , " --toc-depth=1"
                         , " --epub-stylesheet="
                         , pathToCss
                         , " --epub-embed-font="
                         , mainFont
                         , " --epub-embed-font="
                         , codeFontNormal
                         , " --epub-embed-font="
                         , codeFontBold
                         , " "
                         , title
                         , " "
                         , pathToSingleMarkdown
                         ]
  where
    out             = "epub/ohaskell.epub"
    pathToCss       = "epub/EPUB.css"
    title           = "epub/EPUBTitle.txt"
    mainFont        = "/Library/Fonts/PTSerif.ttc"
    codeFontNormal  = "/Users/dshevchenko/Library/Fonts/UbuntuMono-Regular.ttf"
    codeFontBold    = "/Users/dshevchenko/Library/Fonts/UbuntuMono-Bold.ttf"

