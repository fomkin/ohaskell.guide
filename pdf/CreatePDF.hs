{-
 - Create PDF from Markdown using pandoc.
 - Usage:
 - $ cd ohaskell
 - $ stack exec runhaskell CreatePDF.hs
 -}

{-# LANGUAGE OverloadedStrings #-}

module CreatePDF where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Attoparsec.Text
import           Control.Monad
import           System.Process         (callCommand)

main :: IO ()
main = do
    ruTemplate <- TIO.readFile "../templates/default.html"
    let toc = getTOCFrom ruTemplate
        chaptersURLs = getChaptersURLsFrom toc
        mdURLs = createMarkdownURLsFrom chaptersURLs
    chapters <- mapM readMD mdURLs
    TIO.writeFile tmpMD $ T.intercalate "\n" chapters
    callCommand $ concat [ "pandoc --latex-engine=xelatex --include-before-body="
                         , tmpLATEX
                         , " --toc"
                         , " -V mainfont=\"PT Serif\""
                         , " -V monofont=\"Source Code Pro\""
                         , " -V fontsize=12pt"
                         , " -V toc-depth=1"
                         , " -V documentclass=\"book\""
                         , " -V title=\"О Haskell по-человечески\""
                         , " -V author=\"Денис Шевченко\""
                         , " -V polyglossia-lang=\"ru\""
                         , " -o ohaskell.pdf "
                         , tmpMD
                         ]
  where
    tmpLATEX = "header.tex"
    tmpMD = "/tmp/ohaskell-book.md"

getTOCFrom :: T.Text -> T.Text
getTOCFrom ruTemplate = case parseOnly tocParser ruTemplate of
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
    create url = "../chapters" `T.append` (T.replace ".html" ".md" url)

readMD :: T.Text -> IO T.Text
readMD mdURL = TIO.readFile $ T.unpack mdURL >>= return

