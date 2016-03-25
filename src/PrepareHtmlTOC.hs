{-# LANGUAGE OverloadedStrings #-}

module PrepareHtmlTOC (
    polishHtml
) where

import qualified Data.Vector            as V
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Chapters

polishHtml :: IO ()
polishHtml = prepareHtmlTOC >> prepareHtmlPageTitles

prepareHtmlTOC :: IO ()
prepareHtmlTOC = V.mapM_ (handle chaptersURLsWithIndex) chaptersURLsWithIndex
  where
    chaptersURLsWithIndex = V.indexed . V.fromList $ chaptersURLs

handle :: V.Vector (Int, T.Text) -> (Int, T.Text) -> IO ()
handle chaptersURLsWithIndex (currentChapterIndex, currentChapterURL)
    | currentChapterIndex == 0 = handleFirstChapter
    | otherwise = if currentChapterIndex + 1 == V.length chaptersURLsWithIndex
                      then handleLastChapter
                      else handleChapter
  where
    pathToCurrentChapter = "_site" ++ T.unpack currentChapterURL
    initChapter = "/index.html" :: T.Text

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
        let chapter'  = T.replace "PREV_CHAPTER_URL" prev chapter
            chapter'' = T.replace "NEXT_CHAPTER_URL" next chapter'
        TIO.writeFile current chapter''

prepareHtmlPageTitles :: IO ()
prepareHtmlPageTitles = mapM_ prepare chaptersURLsNNames
  where
    prepare :: (T.Text, T.Text) -> IO ()
    prepare (url, name) = do
        let path = "_site" ++ T.unpack url
        chapter <- TIO.readFile path
        let chapter' = T.replace "PAGE_TITLE" name chapter
        TIO.writeFile path chapter'

