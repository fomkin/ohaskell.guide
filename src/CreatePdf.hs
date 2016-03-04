{-# LANGUAGE OverloadedStrings #-}

module CreatePdf (
    createPdf
) where

import System.Process (callCommand)

createPdf :: FilePath -> IO ()
createPdf pathToSingleMarkdown =
    callCommand $ concat [ "pandoc --latex-engine=xelatex --include-before-body="
                         , headerLATEX
                         , " --toc"
                         , " -V mainfont=\"PT Serif\""
                         , " -V monofont=\"Ubuntu Mono\""
                         , " -V fontsize=12pt"
                         , " -V toc-depth=1"
                         , " -V documentclass=\"book\""
                         , " -V title=\"О Haskell по-человечески\""
                         , " -V author=\"Денис Шевченко\""
                         , " -V polyglossia-lang=\"ru\""
                         , " -o "
                         , out
                         , " "
                         , pathToSingleMarkdown
                         ]
  where
    headerLATEX = "pdf/header.tex"
    out         = "pdf/ohaskell.pdf"

