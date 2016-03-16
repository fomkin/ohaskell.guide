{-# LANGUAGE OverloadedStrings #-}

module CreatePdf (
    createPdf
) where

import System.Process (callCommand)

createPdf :: FilePath -> IO ()
createPdf pathToSingleMarkdown =
    callCommand $ concat [ "pandoc --latex-engine=xelatex"
                         , " --include-before-body="
                         , headerLATEX
                         , " --toc"
                         , " -V mainfont=\"PT Serif\""
                         , " -V monofont=\"Ubuntu Mono\""
                         , " -V fontsize=11pt"
                         , " -V toc-depth=1"
                         , " -V documentclass=\"book\""
                         , " -V classoption=\"oneside\""
                         , " -V geometry=\"margin=1.2in\""
                         , " -V geometry=\"headsep=0.5in\""
                         , " -V title=\"О Haskell по-человечески\""
                         , " -V author=\"Денис Шевченко\""
                         , " -V colorlinks=\"true\""
                         , " -V urlcolor=\"Blue\""
                         , " -V polyglossia-lang=\"ru\""
                         , " -V polyglossia-otherlangs=\"[en]\""
                         , " -o "
                         , out
                         , " "
                         , pathToSingleMarkdown
                         ]
  where
    headerLATEX = "pdf/header.tex"
    out         = "pdf/ohaskell.pdf"

