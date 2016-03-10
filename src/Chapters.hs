{-# LANGUAGE OverloadedStrings #-}

module Chapters (
      chaptersURLsNNames
    , chaptersURLs
    , chaptersNames
) where

import qualified Data.Text as T

chaptersURLsNNames :: [(T.Text, T.Text)]
chaptersURLsNNames = [ ("/init.html",                     "Добро пожаловать!")
                     , ("/haskell-faq.html",              "Первые вопросы")
                     , ("/this-book.html",                "Об этой книге")
                     , ("/setup.html",                    "Приготовимся")
                     , ("/whales-n-turtle.html",          "Киты и Черепаха")
                     , ("/immutability-n-purity.html",    "Неизменность и чистота")
                     , ("/if-n-return.html",              "Выбираем и возвращаемся")
                     , ("/choose-n-patterns.html",        "Выбор и образцы")
                     , ("/hackage.html",                  "Hackage")
                     ]

chaptersURLs :: [T.Text]
chaptersURLs = [url | (url, _) <- chaptersURLsNNames]

chaptersNames :: [T.Text]
chaptersNames = [name | (_, name) <- chaptersURLsNNames]

