{-# LANGUAGE OverloadedStrings #-}

module CreatePdf (
    createPdf
) where

import System.Process (callCommand)

createPdf :: FilePath -> IO ()
createPdf pathToSingleMarkdown = createPdfDesktop >> createPdfMobile
  where
    createPdfDesktop = callCommand $ concat [ "pandoc --latex-engine=xelatex"
                                            , " --template="
                                            , template
                                            , " -V fontsize=\"11pt\""
                                            , " -V classoption=\"oneside\""
                                            , " -V geometry=\"margin=1.2in\""
                                            , " -V geometry=\"headsep=0.5in\""
                                            , " --no-highlight"
                                            , " -o "
                                            , outDesktop
                                            , " "
                                            , pathToSingleMarkdown
                                            ]

    createPdfMobile = callCommand $ concat [ "pandoc --latex-engine=xelatex"
                                           , " --template="
                                           , template
                                           , " -V fontsize=\"12pt\""
                                           , " -V classoption=\"oneside\""
                                           , " -V geometry=\"headsep=0.3in\""
                                           , " -V geometry=\"top=0.75in\""
                                           , " -V geometry=\"bottom=0.8in\""
                                           , " -V geometry=\"left=0.3in\""
                                           , " -V geometry=\"right=0.3in\""
                                           , " -V geometry=\"paper=a5paper\""
                                           , " --no-highlight"
                                           , " -o "
                                           , outMobile
                                           , " "
                                           , pathToSingleMarkdown
                                           ]
    template   = "pdf/template.tex"
    outDesktop = "pdf/ohaskell.pdf"
    outMobile  = "pdf/ohaskell-mobile.pdf"

