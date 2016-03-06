module Main where

import SingleMarkdown
import CreatePdf
import CreateEpub
import CreateHtml
import CreateHtmlTemplates

main :: IO ()
main = do
    createHtmlTemplates
    pathToSingleMarkdown <- createSingleMarkdown
    putStrLn "Creating PDF..."  >> createPdf pathToSingleMarkdown
    putStrLn "Creating EPUB..." >> createEpub pathToSingleMarkdown
    putStrLn "Creating HTML..." >> createHtml

