{-# LANGUAGE OverloadedStrings #-}

module CreatePdf (
    createPdf
) where

import System.Process (callCommand)

createPdf :: FilePath -> IO ()
createPdf pathToSingleMarkdown =
    callCommand $ concat [ "pandoc --latex-engine=xelatex"
                         , " --template="
                         , template
                         , " -o "
                         , out
                         , " "
                         , pathToSingleMarkdown
                         ]
  where
    template = "pdf/template.tex"
    out      = "pdf/ohaskell.pdf"

