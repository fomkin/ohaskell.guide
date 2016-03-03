#!/bin/bash

# Собираем веб-версию книги, локально.

stack install
ohaskell rebuild
stack exec runhaskell HandleTOC.hs

cd pdf
stack exec runhaskell CreatePDF.hs
cd ..

cd epub
stack exec runhaskell CreateEPUB.hs
cd ..

# После этого в корне репозитория смотрим в каталог _site.
