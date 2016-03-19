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
                                           , " -V geometry=\"margin=0.3in\""
                                           , " -V geometry=\"headsep=0.3in\""
                                           , " -V geometry=\"top=0.7in\""
                                           , " -V geometry=\"bottom=0.75in\""
                                           , " -V geometry=\"paper=a5paper\""
                                           , " -o "
                                           , outMobile
                                           , " "
                                           , pathToSingleMarkdown
                                           ]
    template   = "pdf/template.tex"
    outDesktop = "pdf/ohaskell.pdf"
    outMobile  = "pdf/ohaskell-mobile.pdf"

