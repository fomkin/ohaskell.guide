#!/bin/bash

# Собираем веб-версию книги, локально.

stack install
ohaskell rebuild
stack exec runhaskell HandleTOC.hs
# stack exec runhaskell CreatePDF.hs

# После этого в корне репозитория смотрим в каталог _site.
