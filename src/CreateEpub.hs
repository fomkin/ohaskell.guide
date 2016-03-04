{-# LANGUAGE OverloadedStrings #-}

module CreateEpub (
    createEpub
) where

import System.Process (callCommand)

createEpub :: FilePath -> IO ()
createEpub pathToSingleMarkdown =
    callCommand $ concat [ "pandoc -S -o "
                         , out
                         , " --toc-depth=1"
                         , " --epub-stylesheet="
                         , css
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
    css             = "epub/EPUB.css"
    title           = "epub/EPUBTitle.txt"
    mainFont        = "/Library/Fonts/PTSerif.ttc"
    codeFontNormal  = "/Users/dshevchenko/Library/Fonts/UbuntuMono-Regular.ttf"
    codeFontBold    = "/Users/dshevchenko/Library/Fonts/UbuntuMono-Bold.ttf"

