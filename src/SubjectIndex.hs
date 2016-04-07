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
      ("case",                              [ ("Выбор и образцы", "case")
                                            ]
      )

    , ("Hackage",                           [ ("Hackage и библиотеки", "Hackage")
                                            ]
      )

    , ("if",                                [ ("Выбираем и возвращаемся", "Выбор и выход")
                                            , ("Выбор и образцы", "Не только из двух")
                                            ]
      )

    , ("let",                               [ ("Пусть будет там, Где...", "Пусть")
                                            ]
      )

    , ("Miranda",                           [ ("Лень", "Для любопытных")
                                            ]
      )

    , ("space leak",                        [ ("Лень", "Space leak")
                                            ]
      )

    , ("stack",                             [ ("Приготовимся", "Устанавливаем")
                                            ]
      )

    , ("thunk",                             [ ("Лень", "Как можно меньше")
                                            , ("Лень", "Space leak")
                                            ]
      )

    , ("undefined",                         [ ("Лень", "Лень и строгость вместе")
                                            ]
      )

    , ("where",                             [ ("Пусть будет там, Где...", "Где")
                                            ]
      )

    , ("WHNF",                              [ ("Лень", "Как можно меньше")
                                            ]
      )

    , ("арифметическая последовательность", [ ("Список", "Перечисление")
                                            ]
      )

    , ("АТД",                               [ ("АТД", "")
                                            ]
      )

    , ("выведение типа",                    [ ("Киты и Черепаха", "Выведение")
                                            ]
      )

    , ("выражение",                         [ ("Киты и Черепаха", "Черепаха")
                                            ]
      )

    , ("жадные/строгие вычисления",         [ ("Лень", "Две модели вычислений")
                                            ]
      )

    , ("история Haskell",                   [ ("Первые вопросы", "Это что какой-то новый язык")
                                            ]
      )

    , ("класс типов",                       [ ("Киты и Черепаха", "Третий Кит")
                                            ]
      )

    , ("конструктор значения",              [ ("Наши типы", "Знакомство")
                                            ]
      )

    , ("кортеж",                            [ ("Кортеж", "")
                                            ]
      )

    , ("ленивые вычисления",                [ ("Лень", "Две модели вычислений")
                                            ]
      )

    , ("лямбда-функция",                    [ ("Лямбда-функция", "Истоки")
                                            , ("ФВП", "Частичное применение")
                                            , ("Композиция функций", "Как работает композиция")
                                            ]
      )

    , ("модуль",                            [ ("Приготовимся", "Модули: знакомство")
                                            , ("Hackage и библиотеки", "")
                                            ]
      )

    , ("нормальная форма",                  [ ("Лень", "Как можно меньше")
                                            ]
      )

    , ("нульарный конструктор",             [ ("Наши типы", "Значение-пустышка")
                                            ]
      )

    , ("объявление функции",                [ ("Неизменность и чистота", "Объявляем и определяем")
                                            ]
      )

    , ("оператор",                          [ ("Мир операторов", "")
                                            ]
      )

    , ("оператор присваивания",             [ ("Неизменность и чистота", "Чисто функциональный")
                                            ]
      )

    , ("определение функции",               [ ("Неизменность и чистота", "Объявляем и определяем")
                                            ]
      )

    , ("оптимизация",                       [ ("Лень", "Оптимизация")
                                            ]
      )

    , ("паттерн матчинг",                   [ ("Выбор и образцы", "Сравнение с образцом")
                                            , ("Кортеж", "Действия над кортежами")
                                            , ("Рекурсия", "Правда о списке")
                                            , ("АТД", "Извлекаем значение")
                                            , ("АТД: поля с метками", "Без меток")
                                            ]
      )

    , ("рекурсия",                          [ ("Рекурсия", "")
                                            ]
      )

    , ("сильная типизация",                 [ ("Киты и Черепаха", "Сила")
                                            ]
      )

    , ("сравнение с образцом",              [ ("Выбор и образцы", "Сравнение с образцом")
                                            , ("Кортеж", "Действия над кортежами")
                                            , ("АТД", "Извлекаем значение")
                                            , ("АТД: поля с метками", "Без меток")
                                            ]
      )

    , ("список",                            [ ("Список", "")
                                            ]
      )

    , ("списочный диапазон",                [ ("Список", "Перечисление")
                                            ]
      )

    , ("статическая типизация",             [ ("Киты и Черепаха", "Статическая проверка")
                                            ]
      )

    , ("тип",                               [ ("Киты и Черепаха", "Второй Кит")
                                            , ("Наши типы", "Знакомство")
                                            ]
      )

    , ("функция высшего порядка",           [ ("ФВП", "")
                                            ]
      )

    , ("частичное применение",              [ ("ФВП", "Частичное применение")
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
    createIdFrom aSectionName = T.strip . T.toLower $ T.replace " " "-" aSectionName

