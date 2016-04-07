[![CircleCI](https://circleci.com/gh/denisshevchenko/ohaskell.guide.svg?style=shield&circle-token=42b4b253957b4896ad05759fce3a7ae576ac8a72)](https://circleci.com/gh/denisshevchenko/ohaskell.guide)&nbsp;&nbsp;&nbsp;[![Code Climate](https://codeclimate.com/github/denisshevchenko/ohaskell.guide/badges/gpa.svg)](https://codeclimate.com/github/denisshevchenko/ohaskell.guide)&nbsp;&nbsp;&nbsp;[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/denisshevchenko/ohaskell-book)

# О Haskell по-человечески

Ваша первая книга об удивительном и прекрасном языке программирования [Haskell](https://www.haskell.org/).

[![readOnline](https://img.shields.io/badge/read-online-blue.svg)](https://www.ohaskell.guide/init.html)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[![getPDF](https://img.shields.io/badge/get-PDF-red.svg)](https://www.ohaskell.guide/pdf/ohaskell.pdf)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[![getPDFMobile](https://img.shields.io/badge/get-PDF%20mobile-orange.svg)](https://www.ohaskell.guide/pdf/ohaskell-mobile.pdf)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[![getEPUB](https://img.shields.io/badge/get-EPUB-green.svg)](https://www.ohaskell.guide/epub/ohaskell.epub)

Книга создана с помощью практичного [Markdown](https://help.github.com/categories/writing-on-github/), блистательного [Materialize](http://materializecss.com/), впечатляющего [Hakyll](https://jaspervdj.be/hakyll/), элегантного [Clay](http://fvisser.nl/clay/), гибкого [BlazeHtml](https://jaspervdj.be/blaze/) и мощного [pandoc](http://pandoc.org/). И разумеется, всё это связано воедино силою Haskell. Книга написана при поддержке [русскоязычного сообщества Haskell-разработчиков](http://ruhaskell.org/).

## Распространение

Книга свободно распространяется на условиях лицензии [CC BY-NC 4.0](http://creativecommons.org/licenses/by-nc/4.0/deed.ru). Исходный программный код ещё более свободно распространяется на условиях лицензии [MIT](https://opensource.org/licenses/MIT).

## Новости

За новостями об обновлениях и исправлениях книги следите в [нашем чате](https://gitter.im/denisshevchenko/ohaskell-book), а также в выпусках подкаста [Бананы и Линзы](http://bananasandlenses.net/), единственного русскоязычного подкаста, всецело посвящённого Haskell. Ну и [Твиттер мой](https://twitter.com/dshevchenko_biz) можете посматривать.

## Локальная сборка

Для локальной сборки вам понадобятся [stack](http://docs.haskellstack.org/en/stable/README/), [pandoc](http://pandoc.org/) и TeX-дистрибутив (я использую [MacTeX](https://tug.org/mactex/)). Делаем:

```bash
$ git clone git@github.com:denisshevchenko/ohaskell.guide.git
$ cd ohaskell.guide
$ stack build
$ stack exec -- ohaskell
```

По умолчанию, при запуске `ohaskell` без аргументов, собираются все варианты книги:

1. HTML: `_site/index.html`.
2. PDF: `pdf/ohaskell.pdf`.
3. PDF, мобильный вариант: `pdf/ohaskell-mobile.pdf`.
4. EPUB: `epub/ohaskell.epub`.

Однако вы можете собрать лишь нужные вам варианты:

1. Для HTML:

```bash
$ stack exec -- ohaskell --html
```

2. Для PDF:

```bash
$ stack exec -- ohaskell --pdf
```

3. Для EPUB:

```bash
$ stack exec -- ohaskell --epub
```

Проверено на OS X Yosemite, [stack 1.0.2](http://docs.haskellstack.org/en/stable/README/), [pandoc 1.15.2.1](https://github.com/jgm/pandoc/releases/tag/1.15.2) и [LTS Haskell 5.5](https://www.stackage.org/lts-5.5).
