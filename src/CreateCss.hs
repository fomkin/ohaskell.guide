{-# LANGUAGE OverloadedStrings #-}

module CreateCss (
    createCss
) where

import           Prelude        hiding (span, div)
import           Clay
import qualified Data.Text.Lazy as L

createCss :: IO ()
createCss = writeFile "static/css/default.css" . L.unpack . render $ do
    let colorBase01   = color "#586e75"
        colorBase03   = color "#002b36"
        base2         = "#eee8d5"
        solarizedBlue = "#268bd2"
        centerAlign   = textAlign $ alignSide sideCenter
        leftAlign     = textAlign $ alignSide sideLeft

        hsCode = code <? do
            color           "#000"
            backgroundColor base2
            textAlign       $ alignSide sideCenter
            paddingTop      $ px 1
            paddingBottom   $ px 1
            paddingLeft     $ px 3
            paddingRight    $ px 3
            fontFamily      ["Ubuntu Mono"] [monospace]
            fontSize        $ pct 100
            borderRadius    (px 3) (px 3) (px 3) (px 3)
            border          solid (px 1) base2

    importUrl "https://fonts.googleapis.com/css?family=PT+Sans:400|PT+Serif:400,700,400italic|Ubuntu+Mono:400,400italic,700&subset=latin,cyrillic"

    body ? do
        colorBase03
        fontSize        $ px 17
        fontFamily      ["PT Serif"] [serif]
        backgroundColor "#fdf6e3"
        textAlign       justify

    h1 ? do
        colorBase01
        fontSize        $ px 30
        centerAlign
        paddingTop      $ px 40
        paddingBottom   $ px 35

    h2 ? do
        colorBase01
        fontSize        $ px 22
        leftAlign
        paddingTop      $ px 30
        paddingBottom   $ px 20

    h3 ? do
        colorBase01
        fontSize        $ px 20
        leftAlign
        paddingTop      $ px 15
        paddingBottom   $ px 10

    p    ? hsCode
    span ? hsCode
    li   ? hsCode

    a ? color solarizedBlue
    a # hover   ? (textDecoration none)
    a # visited ? (textDecoration none)
    a # active  ? (textDecoration none)

    blockquote ? do
        marginLeft      nil
        paddingLeft     $ px 20
        color           "#777"
        borderLeft      solid (px 5) "#ee6e73"

    ".cover-title" ? do
        colorBase01
        fontSize        $ px 48
        paddingTop      $ px 15

    ".cover-v2" ? do
        colorBase01
        fontSize        $ px 22
        paddingTop      $ px 20
        paddingBottom   $ px 70

    ".sans" ? do
        fontFamily ["PT Sans"] [sansSerif]

    ".get-button" ? do
        width $ px 138

    ".get-button-separator" ? do
        paddingBottom $ px 25

    ".smaller-text" ? do
        fontSize $ pct 100

    ".side-nav" ? do
        a ? do
            leftAlign
            fontSize $ px 17
        li ? do
            padding  nil nil nil nil

    nav ? do
        opacity         0.9
        backgroundColor "#2aa198"
        ".brand-logo" ? do
            fontSize $ px 26

        ul ? a ? do
            fontSize $ px 26

    div # "#logo" ? do
        ".navbar-brand" ? do
            fontSize $ px 16

    ".navbar" ? a ? do
        borderBottom    solid nil "#000"
        textDecoration  none

    ".navbar-header" ? a ? do
        borderBottom    solid nil "#000"
        textDecoration  none

    ".navbar-default" ? do
        backgroundColor "#fff"
        borderColor     "#e7e7e7"
        opacity         0.91

        ".navbar-nav" |> li |> a # hover ? do
            color "#222"

        ".navbar-nav" |> li |> a # focus ? do
            color "#222"

    ".author" ? do
        paddingLeft $ px 15

    ".chapter-arrow" ? do
        (fontSize $ px 21)

    ".sourceCode" ? code ? do
        fontFamily ["Ubuntu Mono"] [monospace]

    pre # ".sourceCode" ? do
        color           "#000"
        fontFamily      ["Ubuntu Mono"] [monospace]
        fontSize        $ pct 100
        borderRadius    (px 3) (px 3) (px 3) (px 3)
        border          solid (px 1) base2
        backgroundColor base2
        padding         (px 7) (px 7) (px 7) (px 7)
        marginTop       (px 18)
        marginBottom    (px 18)

        span # ".kw" ? do color "#007020"; fontWeight bold
        span # ".dt" ? color "#902000"
        span # ".dv" ? color "#40a070"
        span # ".bn" ? color "#40a070"
        span # ".fl" ? color "#40a070"
        span # ".ch" ? color "#4070a0"
        span # ".st" ? color "#4070a0"
        span # ".co" ? do color "#60a0b0"; fontStyle italic
        span # ".ot" ? color "#007020"
        span # ".al" ? do color red; fontWeight bold
        span # ".fu" ? color "#06287e"
        span # ".er" ? do color red; fontWeight bold

