{-# LANGUAGE OverloadedStrings #-}

module SingleMarkdown (
    createSingleMarkdown
) where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Chapters

createSingleMarkdown :: IO FilePath
createSingleMarkdown = do
    chapters <- mapM readMarkdownFile createMarkdownURLs
    TIO.writeFile pathToSingleMarkdown $ T.intercalate "\n" chapters
    return pathToSingleMarkdown
  where
    pathToSingleMarkdown = "/tmp/ohaskell-book.md"

createMarkdownURLs :: [T.Text]
createMarkdownURLs = map create chaptersURLs
  where
    create url = "chapters" `T.append` T.replace ".html" ".md" url

readMarkdownFile :: T.Text -> IO T.Text
readMarkdownFile = TIO.readFile . T.unpack

