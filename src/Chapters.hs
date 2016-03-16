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
                     , ("/let-n-where.html",              "Пусть будет там, Где...")
                     , ("/operators.html",                "Мир операторов")
                     , ("/list.html",                     "Список: знакомство")
                     , ("/tuple.html",                    "Кортеж")
                     -- , ("/.html",                    "Генераторы списков") 4
                     -- , ("/.html",                    "Лямбда") 3
                     -- , ("/.html",                    "Применение функций") 3
                     -- , ("/.html",                    "ФВП") 3
                     -- , ("/.html",                    "Hackage") 2
                     -- , ("/.html",                    "Модули: углубляемся") 2
                     -- , ("/.html",                    "Тип: создаём") 3
                     -- , ("/.html",                    "АДТ") 4
                     -- , ("/.html",                    "Конструктор типа") 5
                     -- , ("/.html",                    "Практика: фантомный тип") 5
                     -- , ("/.html",                    "Функтор")
                     -- , ("/.html",                    "Аппликативный функтор") 10
                     -- , ("/.html",                    "Монада") 10
                     -- , ("/.html",                    "Монадный трансформер") 10
                     -- , ("/.html",                    "Stackage") 3
                     -- , ("/.html",                    "форматирование") 3
                     -- , ("/.html",                    "На десерт: демистификация") 3
                     ]

chaptersURLs :: [T.Text]
chaptersURLs = [url | (url, _) <- chaptersURLsNNames]

chaptersNames :: [T.Text]
chaptersNames = [name | (_, name) <- chaptersURLsNNames]

