{-# LANGUAGE OverloadedStrings #-}

module Chapters (
      chaptersURLsNNames
    , chaptersURLs
    , chaptersNames
) where

import qualified Data.Text as T

chaptersURLsNNames :: [(T.Text, T.Text)]
chaptersURLsNNames = [ ("/haskell-faq.html",              "Первые вопросы")
                     , ("/this-book.html",                "Об этой книге")
                     , ("/setup.html",                    "Приготовимся")
                     , ("/modules-minimum.html",          "Модули: знакомство")
                     , ("/hackage.html",                  "Hackage")
                     , ("/expressions-n-functions.html",  "Мир выражений и функций")
                     , ("/immutability-n-purity.html",    "Неизменность и чистота")
                     ]

chaptersURLs :: [T.Text]
chaptersURLs = [url | (url, _) <- chaptersURLsNNames]

chaptersNames :: [T.Text]
chaptersNames = [name | (_, name) <- chaptersURLsNNames]

