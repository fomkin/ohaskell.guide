{-# LANGUAGE OverloadedStrings #-}

module Main where

import Copiers
import Chapters

import Hakyll

main :: IO ()
main = hakyll $ do
    justCopy          "static/images/*"
    justCopy          "static/css/*"
    justCopy          "static/js/*"
    justCopy          "static/fonts/**"
    justCopy          "README.md"
    justCopy          "CNAME"
    justCopy          "LICENSE"
    justCreateAndCopy ".nojekyll"

    prepareTemplates >> createCoverPage >> createInitPage >> createChapters

