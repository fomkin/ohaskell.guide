module Main where

import           SingleMarkdown
import           CreatePdf
import           CreateEpub
import           CreateHtml
import           CreateHtmlTemplates

import           Control.Monad              (when)
import           Control.Concurrent.Async   (async, wait)
import           System.Environment         (getArgs, withArgs)
import           System.Exit                (exitFailure)

main :: IO ()
main = do
    (pathToSingleMarkdown, chapterPoints) <- createSingleMarkdown

    args <- getArgs
    check args

    buildEpubIfNecessary args pathToSingleMarkdown
    buildPdfIfNecessary  args pathToSingleMarkdown
    buildHtmlIfNecessary args chapterPoints

buildEpubIfNecessary :: [String] -> FilePath -> IO ()
buildEpubIfNecessary args pathToSingleMarkdown =
    when (buildAllOrJust epub args) $ do
        buildingVersion epub
        createEpub pathToSingleMarkdown

buildPdfIfNecessary :: [String] -> FilePath -> IO ()
buildPdfIfNecessary args pathToSingleMarkdown =
    when (buildAllOrJust pdf args) $ do
        -- Самые тяжеловесные операции.
        buildingVersion pdf
        pdfDesktopDone <- async $ createPdfDesktop pathToSingleMarkdown
        pdfMobileDone  <- async $ createPdfMobile pathToSingleMarkdown
        wait pdfDesktopDone
        wait pdfMobileDone

buildHtmlIfNecessary :: [String] -> [ChapterPoint] -> IO ()
buildHtmlIfNecessary args chapterPoints =
    when (buildAllOrJust html args) $ do
        buildingVersion html
        createHtmlTemplates chapterPoints
        -- Аргумент rebuild нужен для Hakyll.
        withArgs ["rebuild"] $ createHtml chapterPoints

check :: [String] -> IO ()
check args =
    when someInvalidArgs $ do
        putStrLn $ "Usage: ohaskell ["  ++ pdf ++
                                    "|" ++ epub ++
                                    "|" ++ html ++ "]"
        exitFailure
  where
    someInvalidArgs = not . null $ filter invalid args
    invalid arg     =    arg /= pdf
                      && arg /= epub
                      && arg /= html

buildAllOrJust :: String -> [String] -> Bool
buildAllOrJust some args = some `elem` args || null args

pdf, epub, html :: String
pdf  = "--pdf"
epub = "--epub"
html = "--html"

buildingVersion :: String -> IO ()
buildingVersion ver = putStrLn $ " Build " ++ drop 2 ver ++ " version..."

