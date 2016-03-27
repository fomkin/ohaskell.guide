{-# LANGUAGE OverloadedStrings #-}

module PrepareHtmlTOC (
    polishHtml
) where

import qualified Data.Vector            as V
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           SingleMarkdown

polishHtml :: [ChapterPoint] -> IO ()
polishHtml chapterPoints = do
    prepareHtmlTOC chapterPoints
    prepareHtmlPageTitles chapterPoints

prepareHtmlTOC :: [ChapterPoint] -> IO ()
prepareHtmlTOC chapterPoints = V.mapM_ (handle chaptersURLsWithIndex) chaptersURLsWithIndex
  where
    chaptersURLsWithIndex = V.indexed . V.fromList $ [path | (_, path) <- chapterPoints]

handle :: V.Vector (Int, ChapterPath) -> (Int, ChapterPath) -> IO ()
handle chaptersURLsWithIndex (currentChapterIndex, currentChapterURL)
    | currentChapterIndex == 0 = handleFirstChapter
    | otherwise = if currentChapterIndex + 1 == V.length chaptersURLsWithIndex
                      then handleLastChapter
                      else handleChapter
  where
    pathToCurrentChapter = "_site" ++ currentChapterURL
    initChapter = "/index.html" :: FilePath

    handleFirstChapter =
        let prevChapterURL      = initChapter
            (_, nextChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex + 1)
        in replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    handleChapter =
        let (_, prevChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex - 1)
            (_, nextChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex + 1)
        in replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    handleLastChapter =
        let (_, prevChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex - 1)
            nextChapterURL      = initChapter
        in replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    replaceInChapter prev next current = do
        chapter <- TIO.readFile current
        let chapter'  = T.replace "PREV_CHAPTER_URL" (T.pack prev) chapter
            chapter'' = T.replace "NEXT_CHAPTER_URL" (T.pack next) chapter'
        TIO.writeFile current chapter''

prepareHtmlPageTitles :: [ChapterPoint] -> IO ()
prepareHtmlPageTitles = mapM_ prepare
  where
    prepare :: (ChapterName, ChapterPath) -> IO ()
    prepare (name, rawPath) = do
        let path = "_site" ++ rawPath
        chapter <- TIO.readFile path
        let chapter' = T.replace "PAGE_TITLE" name chapter
        TIO.writeFile path chapter'

