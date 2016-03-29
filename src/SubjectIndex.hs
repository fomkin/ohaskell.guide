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
      ("stack",                     [ ("Приготовимся", "Устанавливаем")
                                    ]
      )
    , ("Hackage",                   [ ("Hackage и библиотеки", "Hackage")
                                    ]
      )
    , ("лямбда-функция",            [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("оператор",                  [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("функция высшего порядка",   [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("оператор присваивания",     [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("модуль",                    [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("cabal",                     [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("case",                      [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("if",                        [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("история Haskell",           [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("объявление функции",        [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("определение функции",       [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("let",                       [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("where",                     [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("монада",                    [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("функтор",                   [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("аппликативный функтор",     [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("монадный трансформер",      [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("частичное применение",      [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("IO",                        [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("АТД",                       [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("GADT",                      [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("рекурсия",                  [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("список",                    [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("кортеж",                    [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("генератор списка",          [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("списочный диапазон",        [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("конструктор значения",      [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("конструктор типа",          [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("фантомный тип",             [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
    , ("newtype",                   [ ("АТД: поля с метками", "Без меток")
                                    , ("АТД: поля с метками", "И ещё")
                                    ]
      )
  ]

------------------------------------------------------------------------

subjectIndexWithHrefs :: [ChapterPoint] -> [SubjectItemWithHref]
subjectIndexWithHrefs chapterPoints = map (create chapterPoints) subjectIndex

create :: [ChapterPoint] -> SubjectItem -> SubjectItemWithHref
create chapterPoints (subjectName, links) =
    let hrefs           = map (createHrefFromLink chapterPoints) links
        hrefsWithIndex  = V.indexed . V.fromList $ hrefs
        hrefsWithLabels = V.map makeLabel hrefsWithIndex
    in (subjectName, V.toList hrefsWithLabels)

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

