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

import           Chapters

type ChapterName = T.Text
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
      ("stack",                   [ ("Приготовимся", "Устанавливаем")
                                  ]
      )
    , ("Hackage",                 [ ("Hackage и библиотеки", "Hackage")
                                  ]
      )
    , ("лямбда-функция",          [ ("АТД: поля с метками", "Без меток")
                                  , ("АТД: поля с метками", "И ещё")
                                  ]
      )
    , ("оператор",                [ ("АТД: поля с метками", "Без меток")
                                  , ("АТД: поля с метками", "И ещё")
                                  ]
      )
    , ("функция высшего порядка", [ ("АТД: поля с метками", "Без меток")
                                  , ("АТД: поля с метками", "И ещё")
                                  ]
      )
    , ("оператор присваивания",   [ ("АТД: поля с метками", "Без меток")
                                  , ("АТД: поля с метками", "И ещё")
                                  ]
      )
  ]

------------------------------------------------------------------------

subjectIndexWithHrefs :: [SubjectItemWithHref]
subjectIndexWithHrefs = map create subjectIndex

create :: SubjectItem -> SubjectItemWithHref
create (subjectName, links) =
    let hrefs           = map createHrefFromLink links
        hrefsWithIndex  = V.indexed . V.fromList $ hrefs
        hrefsWithLabels = V.map makeLabel hrefsWithIndex
    in (subjectName, V.toList hrefsWithLabels)

makeLabel :: (Int, Href) -> HrefWithLabel
makeLabel (index, href) = (href, roman)
  where
    roman = toRoman (index + 1) :: T.Text

createHrefFromLink :: Link -> Href
createHrefFromLink (chapterName, sectionName) =
    case chapterURLByName chapterName of
        Nothing  -> error $ "No such chapter '" ++ T.unpack chapterName ++ "', abort!"
        Just url -> url `T.append` "#" `T.append` createIdFrom sectionName
  where
    createIdFrom aSectionName = T.toLower $ T.replace " " "-" aSectionName

