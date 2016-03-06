{-# LANGUAGE OverloadedStrings #-}

module SingleMarkdown (
    createSingleMarkdown
) where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Chapters

createSingleMarkdown :: IO FilePath
createSingleMarkdown = do
    chapters <- mapM readMD $ createMarkdownURLs
    TIO.writeFile pathToSingleMarkdown $ T.intercalate "\n" chapters
    return pathToSingleMarkdown
  where
    pathToSingleMarkdown :: FilePath
    pathToSingleMarkdown = "/tmp/ohaskell-book.md"

createMarkdownURLs :: [T.Text]
createMarkdownURLs = map create chaptersURLs
  where
    create url = "chapters" `T.append` (T.replace ".html" ".md" url)

readMD :: T.Text -> IO T.Text
readMD mdURL = TIO.readFile $ T.unpack mdURL >>= return

