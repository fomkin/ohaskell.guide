module Main where

import TOC
import CreatePdf
import CreateEpub
import CreateHtml

main :: IO ()
main = do
    pathToSingleMarkdown <- createSingleMarkdown
    putStrLn "Creating PDF..."  >> createPdf pathToSingleMarkdown
    putStrLn "Creating EPUB..." >> createEpub pathToSingleMarkdown
    putStrLn "Creating HTML..." >> createHtml

