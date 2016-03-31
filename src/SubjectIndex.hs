{-# LANGUAGE OverloadedStrings #-}

module SubjectIndex (
      subjectIndexWithHrefs
    , SubjectName
    , Href
    , HrefWithLabel
) where

import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Numeral.Roman     (toRoman)

import           SingleMarkdown

type SectionName = T.Text
type SubjectName = T.Text
type Link        = (ChapterName, SectionName)
type SubjectItem = (SubjectName, [Link])

type Href = T.Text
type HrefWithLabel = (Href, T.Text)
type SubjectItemWithHref = (SubjectName, [HrefWithLabel])

------------------------------------------------------------------------

subjectIndex :: [SubjectItem]
subjectIndex =
  [
      ("stack",                             [ ("Приготовимся", "Устанавливаем")
                                            ]
      )

    , ("Hackage",                           [ ("Hackage и библиотеки", "Hackage")
                                            ]
      )

    , ("лямбда-функция",                    [ ("Лямбда-функция", "Истоки")
                                            , ("ФВП", "Частичное применение")
                                            , ("Композиция функций", "Как работает композиция")
                                            ]
      )

    , ("оператор",                          [ ("Мир операторов", "")
                                            ]
      )

    , ("функция высшего порядка",           [ ("ФВП", "")
                                            ]
      )

    , ("оператор присваивания",             [ ("Неизменность и чистота", "Чисто функциональный")
                                            ]
      )

    , ("модуль",                            [ ("Приготовимся", "Модули: знакомство")
                                            , ("Hackage и библиотеки", "")
                                            ]
      )

    , ("case",                              [ ("Выбор и образцы", "case")
                                            ]
      )

    , ("if",                                [ ("Выбираем и возвращаемся", "Выбор и выход")
                                            , ("Выбор и образцы", "Не только из двух")
                                            ]
      )

    , ("история Haskell",                   [ ("Первые вопросы", "Это что какой-то новый язык")
                                            ]
      )

    , ("объявление функции",                [ ("Неизменность и чистота", "Объявляем и определяем")
                                            ]
      )

    , ("определение функции",               [ ("Неизменность и чистота", "Объявляем и определяем")
                                            ]
      )

    , ("let",                               [ ("Пусть будет там, Где...", "Пусть")
                                            ]
      )

    , ("where",                             [ ("Пусть будет там, Где...", "Где")
                                            ]
      )

    , ("частичное применение",              [ ("ФВП", "Частичное применение")
                                            ]
      )

    , ("АТД",                               [ ("АТД", "")
                                            ]
      )

    , ("список",                            [ ("Список", "")
                                            ]
      )

    , ("кортеж",                            [ ("Кортеж", "")
                                            ]
      )

    , ("списочный диапазон",                [ ("Список", "Перечисление")
                                            ]
      )

    , ("арифметическая последовательность", [ ("Список", "Перечисление")
                                            ]
      )

    , ("конструктор значения",              [ ("Наши типы", "Знакомство")
                                            ]
      )

    , ("нульарный конструктор",             [ ("Наши типы", "Значение-пустышка")
                                            ]
      )
  ]

------------------------------------------------------------------------

subjectIndexWithHrefs :: [ChapterPoint] -> [SubjectItemWithHref]
subjectIndexWithHrefs chapterPoints = map (create chapterPoints) subjectIndex

create :: [ChapterPoint] -> SubjectItem -> SubjectItemWithHref
create chapterPoints (subjectName, links) = (subjectName, V.toList hrefsWithLabels)
  where
    hrefs           = map (createHrefFromLink chapterPoints) links
    hrefsWithIndex  = V.indexed . V.fromList $ hrefs
    hrefsWithLabels = V.map makeLabel hrefsWithIndex

makeLabel :: (Int, Href) -> HrefWithLabel
makeLabel (index, href) = (href, roman)
  where
    roman = toRoman (index + 1) :: T.Text

createHrefFromLink :: [ChapterPoint] -> Link -> Href
createHrefFromLink chapterPoints (chapterName, sectionName) =
    case lookup chapterName chapterPoints of
        Nothing  -> error $ "No such chapter '" ++ T.unpack chapterName ++ "', abort!"
        Just url -> T.pack url `T.append` "#" `T.append` createIdFrom sectionName
  where
    createIdFrom aSectionName = T.toLower $ T.replace " " "-" aSectionName

