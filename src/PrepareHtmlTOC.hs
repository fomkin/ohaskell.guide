{-# LANGUAGE OverloadedStrings #-}

module PrepareHtmlTOC (
    prepareHtmlTOC
) where

import qualified Data.Vector            as V
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           TOC

prepareHtmlTOC :: IO ()
prepareHtmlTOC = do
    template <- TIO.readFile mainTemplate
    let toc = getTOCFrom template
        chaptersURLs = getChaptersURLsFrom toc
        chaptersURLsWithIndex = V.indexed . V.fromList $ chaptersURLs
    V.mapM_ (handle chaptersURLsWithIndex) chaptersURLsWithIndex

handle :: V.Vector (Int, T.Text) -> (Int, T.Text) -> IO ()
handle chaptersURLsWithIndex (currentChapterIndex, currentChapterURL) = do
    if currentChapterIndex == 0
       then handleFirstChapter
       else if currentChapterIndex + 1 == V.length chaptersURLsWithIndex
               then handleLastChapter
               else handleChapter
  where
    pathToCurrentChapter = "_site" ++ T.unpack currentChapterURL
    initChapter = "/index.html" :: T.Text

    handleFirstChapter = do
        let prevChapterURL      = initChapter
            (_, nextChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex + 1)
        replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    handleChapter = do
        let (_, prevChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex - 1)
            (_, nextChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex + 1)
        replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    handleLastChapter = do
        let (_, prevChapterURL) = chaptersURLsWithIndex V.! (currentChapterIndex - 1)
            nextChapterURL      = initChapter
        replaceInChapter prevChapterURL nextChapterURL pathToCurrentChapter

    replaceInChapter prev next current = do
        chapter <- TIO.readFile current
        let chapter'  = T.replace "PREV_CHAPTER_URL" prev chapter
            chapter'' = T.replace "NEXT_CHAPTER_URL" next chapter'
        TIO.writeFile current chapter''
