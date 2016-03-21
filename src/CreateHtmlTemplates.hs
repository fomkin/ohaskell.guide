{-# LANGUAGE OverloadedStrings #-}

module CreateHtmlTemplates (
    createHtmlTemplates
) where

import           Text.Blaze.Html5                   as H
import           Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Internal                (attribute)
import qualified Data.Text                          as T

import           Chapters

createHtmlTemplates :: IO ()
createHtmlTemplates = do
    writeFile "templates/cover.html"   $ renderHtml createCover
    writeFile "templates/default.html" $ renderHtml createDefault
    writeFile "templates/chapter.html" $ renderHtml createChapter

createCover :: Html
createCover = docTypeHtml ! lang "ru" $ do
    commonHead "О Haskell по-человечески"

    body $ do
        nav $
            H.div ! class_ "nav-wrapper" $ do
                H.div ! class_ "left author sans" $ do
                    preEscapedToHtml ("&copy; 2016&nbsp;" :: String)
                    a ! href "http://dshevchenko.biz" ! target "_blank" $
                        "Д. Шевченко"

                contacts

        H.div ! class_ "container" $
            H.div ! class_ "row center" ! A.style "padding-top: 70px;" $ do
                H.div ! class_ "cover-title" $
                    "О Haskell по-человечески"

                H.div ! class_ "cover-v2" $
                    "издание 2.0"

                a ! class_ "waves-effect waves-light btn btn-large blue accent-2 get-button sans"
                  ! href "/init.html" $ do
                    H.span ! class_ "sans" ! A.style "text-transform: none;" $ "Web"
                    H.span ! A.style "padding-right: 15px;" $ ""
                    H.i ! class_ "fa fa-cloud" ! A.style "font-size: 20px;" $ ""

                H.div ! class_ "get-button-separator" $ ""

                a ! class_ "waves-effect waves-light btn btn-large red darken-1 get-button sans"
                  ! href "https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell.pdf?raw=true" $ do
                    H.span ! class_ "sans" $ "PDF"
                    H.span ! A.style "padding-right: 17px;" $ ""
                    H.i ! class_ "fa fa-desktop" ! A.style "font-size: 20px;" $ ""

                H.div ! class_ "get-button-separator" $ ""

                a ! class_ "waves-effect waves-light btn btn-large red lighten-1 get-button sans"
                  ! href "https://github.com/denisshevchenko/ohaskell.guide/blob/master/pdf/ohaskell-mobile.pdf?raw=true" $ do
                    H.span ! class_ "sans" $ "PDF"
                    H.span ! A.style "padding-right: 26px;" $ ""
                    H.i ! class_ "fa fa-tablet" ! A.style "font-size: 20px;" $ ""

                H.div ! class_ "get-button-separator" $ ""

                a ! class_ "waves-effect waves-light btn btn-large light-green darken-1 get-button sans"
                  ! href "https://github.com/denisshevchenko/ohaskell.guide/blob/master/epub/ohaskell.epub?raw=true" $ do
                    H.span ! class_ "sans" $ "EPUB"
                    H.span ! A.style "padding-right: 10px;" $ ""
                    H.i ! class_ "fa fa-book" ! A.style "font-size: 20px;" $ ""

                H.div ! class_ "get-button-separator" $ ""

createDefault :: Html
createDefault = docTypeHtml ! lang "ru" $ do
    commonHead "PAGE_TITLE <- О Haskell по-человечески"

    body $ do
        H.div ! class_ "navbar-fixed" $
            nav $
                H.div ! class_ "nav-wrapper" $ do
                    a ! class_ "brand-logo center sans"
                      ! href "/" $ "#ohaskell"

                    a ! href "#"
                      ! dataActivates "mobile-demo"
                      ! class_ "button-collapse show-on-large"
                      ! A.style "padding-left: 15px;" $
                        H.span ! class_ "fa fa-list-ul" ! A.style "font-size: 26px;" $ ""

                    -- Build side TOC
                    ul ! class_ "side-nav sans" ! A.id "mobile-demo" $
                        mapM_ chapterPoint chaptersURLsNNames

                    contacts

        H.div ! class_ "container" $
            preEscapedToHtml ("$body$" :: String)
  where
    dataActivates :: AttributeValue -> Attribute
    dataActivates = attribute "data-activates" " data-activates=\""

    chapterPoint :: (T.Text, T.Text) -> Html
    chapterPoint (anUrl, aName) = li $ a ! href (textValue anUrl) $ toHtml aName

createChapter :: Html
createChapter = do
    preEscapedToHtml ("$body$" :: String)

    H.div ! A.style "padding-top: 45px;" $ ""

    H.div ! class_ "row" $ do
        H.div ! class_ "col s3" $
            H.div ! class_ "left" $
                a ! href "PREV_CHAPTER_URL"
                  ! class_ "btn waves-effect waves-light chapter-arrow" $
                    H.span ! class_ "fa fa-angle-double-left" $ ""

        H.div ! class_ "col s6" $
            H.div ! class_ "center-align" $
                button ! class_ "waves-effect waves-light btn blue lighten-2 show-comments sans" $
                    "Обсудим?"

        H.div ! class_ "col s3" $
            H.div ! class_ "right" $
                a ! href "NEXT_CHAPTER_URL"
                  ! class_ "btn waves-effect waves-light chapter-arrow" $
                    H.span ! class_ "fa fa-angle-double-right" $ ""

    H.div ! A.style "padding-top: 20px;" $ ""

    -- The empty element required for Disqus to loads comments into
    H.div ! A.id "disqus_thread" ! A.style "padding-top: 30px;" $ ""

--
commonHead :: T.Text -> Html
commonHead customTitle = H.head $ do
    meta ! charset "utf-8"
    meta ! name "description" ! content "О Haskell по-человечески. Ваша первая книга о прекрасном и удивительном языке программирования."
    meta ! name "author" ! content "Денис Шевченко"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1.0"

    H.title $ toHtml customTitle

    link ! rel "icon" ! href "/static/images/favicon.ico"
    link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
    script ! src "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js" $ ""
    link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css"
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js" $ ""
    link ! rel "stylesheet" ! href "/static/css/default.css"
    script ! src "/static/js/default.js" $ ""

-- Author's contacts at up right corner.
contacts :: Html
contacts =
    ul ! A.id "nav-mobile" ! class_ "right" $ do
        li $
            a ! href "https://github.com/denisshevchenko/ohaskell.guide"
              ! target "_blank" $
                H.span ! class_ "fa fa-github" $ ""
        li $
            a ! href "mailto:me@dshevchenko.biz?Subject=#ohaskell,%20О%20книге"
              ! A.title "Написать автору" $
                H.span ! class_ "fa fa-envelope-o" $ ""

