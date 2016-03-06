{-# LANGUAGE OverloadedStrings #-}

module CreateEpubCss (
    createEpubCss
) where

import           Prelude            hiding (span, div)
import           Clay
import qualified Data.Text.Lazy     as L

createEpubCss :: FilePath -> IO ()
createEpubCss pathToCss = writeFile pathToCss . L.unpack . render $ do
    let centerAlign     = textAlign $ alignSide sideCenter
        leftAlign       = textAlign $ alignSide sideLeft
        fontPTSerif     = fontFamily ["PT Serif"] [serif]
        fontUbuntuMono  = fontFamily ["Ubuntu Mono"] [monospace]

    fontFace $ do
        fontPTSerif
        fontStyle       normal
        fontWeight      normal
        fontFaceSrc     [FontFaceSrcUrl "PTSerif.ttc" Nothing]

    fontFace $ do
        fontPTSerif
        fontStyle       normal
        fontWeight      bold
        fontFaceSrc     [FontFaceSrcUrl "PTSerif.ttc" Nothing]

    fontFace $ do
        fontUbuntuMono
        fontStyle       normal
        fontWeight      normal
        fontFaceSrc     [FontFaceSrcUrl "UbuntuMono-Regular.ttf" Nothing]

    fontFace $ do
        fontUbuntuMono
        fontStyle       normal
        fontWeight      bold
        fontFaceSrc     [FontFaceSrcUrl "UbuntuMono-Bold.ttf" Nothing]

    body ? do
        margin          (pct 5) (pct 5) (pct 5) (pct 5)
        textAlign       justify
        fontSize        $ px 11
        fontFamily      ["PT Serif"] [serif]

    code ? do
        fontUbuntuMono

    h1 ? do
        leftAlign
        fontSize        $ px 20
        paddingBottom   $ px 18

    h2 ? do
        leftAlign
        fontSize        $ px 15
        paddingTop      $ px 18

    h3 ? do
        leftAlign

    h1 # ".main" ? do
        centerAlign
        fontSize        $ px 26
        paddingTop      $ px 50

    h1 # ".edition" ? do
        centerAlign
        fontSize        $ px 13
        paddingBottom   $ px 50

    h2 # ".author" ? do
        centerAlign
        fontSize        $ px 13

    ol # ".toc" ? do
        padding         nil nil nil nil
        marginLeft      $ em 1

    ol # ".toc" ? li ? do
        listStyleType none
        margin      nil nil nil nil
        padding     nil nil nil nil

