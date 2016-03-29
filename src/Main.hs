module Main where

import SingleMarkdown
import CreatePdf
import CreateEpub
import CreateHtml
import CreateHtmlTemplates

import Control.Concurrent.Async

main :: IO ()
main = do
    putStrLn "Build a new version of the book, be patient..."

    (pathToSingleMarkdown, chapterPoints) <- createSingleMarkdown

    createHtmlTemplates chapterPoints

    pdfDesktopDone <- async $ createPdfDesktop pathToSingleMarkdown
    pdfMobileDone  <- async $ createPdfMobile pathToSingleMarkdown
    epubDone       <- async $ createEpub pathToSingleMarkdown
    htmlDone       <- async $ createHtml chapterPoints

    wait pdfDesktopDone
    wait pdfMobileDone
    wait epubDone
    wait htmlDone

