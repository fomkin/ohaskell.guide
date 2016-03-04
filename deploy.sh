#!/bin/bash

# Скрипт для для автоматического обновления сайта на GitHub Pages.

USAGE="
Запускаем так: ./deploy.sh \"Сообщение о коммите\"

Пример:
  ./deploy.sh \"Обновление стиля.\"
"

# При любой ошибке скрипт вылетает...
set -e

# Устанавливаем переменную, для нашего коммит-сообщения...
COMMIT_MESSAGE=$1

echo "Собираем новую версию сайта..."
ohaskell rebuild

echo "Учитываем изменения в ветке master..."
if [ "$1" != "" ]
then
    git add .
    git commit -a -m "$COMMIT_MESSAGE"
    git push origin master
fi

echo "Копируем во временное место, предварительно удалив старое, если нужно..."
rm -rf /tmp/_site || true 1> /dev/null
cp -R _site /tmp

echo "Переключаемся на ветку 'gh-pages'..."
git checkout gh-pages

rm -f  *.html
rm -rf static
rm -f  *.md
rm -f  *.hs
rm -f  *.cabal

echo "Копируем прямо в корень содержимое подготовленного каталога _site..."
cp -R /tmp/_site/* .

rm -rf chapters
rm -rf src
rm -rf templates
rm -rf epub
rm -rf pdf
rm -rf _site
rm -rf _cache

echo "Учитываем все последние новшества и публикуем на GitHub Pages..."
git add .
if [ "$1" != "" ]
then
    git commit -a -m "$COMMIT_MESSAGE"
else
    git commit -a -m "Обновление после слияния."
fi

git push origin gh-pages

echo "Возвращаемся в мастер..."
git checkout master

echo "Готово!"
