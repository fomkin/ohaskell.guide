{-# LANGUAGE OverloadedStrings #-}

module TOC (
      createSingleMarkdown
    , getTOCFrom
    , getChaptersURLsFrom
    , substringParser
    , pathToSingleMarkdown
    , mainTemplate
) where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Attoparsec.Text

createSingleMarkdown :: IO FilePath
createSingleMarkdown = do
    mdURLs <- obtainMarkdownTOC
    chapters <- mapM readMD mdURLs
    TIO.writeFile pathToSingleMarkdown $ T.intercalate "\n" chapters
    return pathToSingleMarkdown

pathToSingleMarkdown :: FilePath
pathToSingleMarkdown = "/tmp/ohaskell-book.md"

mainTemplate :: FilePath
mainTemplate = "templates/default.html"

obtainMarkdownTOC :: IO [T.Text]
obtainMarkdownTOC = do
    template <- TIO.readFile mainTemplate
    let rawTOC         = getTOCFrom template
        chaptersURLs   = getChaptersURLsFrom rawTOC
        chaptersURLsMD = createMarkdownURLsFrom chaptersURLs
    return chaptersURLsMD

getTOCFrom :: T.Text -> T.Text
getTOCFrom template = case parseOnly tocParser template of
    Left  _   -> ""
    Right toc -> toc
  where
    tocParser :: Parser T.Text
    tocParser = substringParser '%'

getChaptersURLsFrom :: T.Text -> [T.Text]
getChaptersURLsFrom toc = case parseOnly urlsParser toc of
    Left  _    -> []
    Right urls -> urls
  where
    urlsParser :: Parser [T.Text]
    urlsParser = many1 (substringParser '"')

substringParser :: Char -> Parser T.Text
substringParser ch = do
    skipWhile (/= ch)
    ss <- char ch *> manyTill' anyChar (char ch)
    return . T.pack $ ss

createMarkdownURLsFrom :: [T.Text] -> [T.Text]
createMarkdownURLsFrom chaptersURLs = map create chaptersURLs
  where
    create :: T.Text -> T.Text
    create url = "chapters" `T.append` (T.replace ".html" ".md" url)

readMD :: T.Text -> IO T.Text
readMD mdURL = TIO.readFile $ T.unpack mdURL >>= return

