----
title: Может быть
prevChapter: /ru/delicious/monads-practice.html
nextChapter: /ru/delicious/functors.html
----

Есть в Haskell ещё один механизм обработки ошибок, не связанный ни с исключениями, ни с `IO`. Используется он весьма часто, поэтому знать о нём нужно. Речь идёт о стандартном типе `Maybe`.

## Что за зверь

Тип `Maybe` - это опциональное значение. Оно говорит нам: "Может быть во мне есть некое реальное значение, а может быть и нету". Вот его определение:
 
```haskell
data Maybe a = Nothing | Just a
               deriving (Eq, Ord)
```

Тип представлен двумя конструкторами: нульарным `Nothing` и унарным `Just`. Если значение было создано с конструктором `Nothing` - значит оно представляет собой пустышку, если же с конструктором `Just` - оно содержит в себе нечто полезное.

## Для чего он

Простой пример:

```haskell
import Data.Char

coefficientFromString :: String -> Maybe Int
coefficientFromString str =
    if isNumber firstChar then Just (digitToInt firstChar) else Nothing
    where firstChar = str !! 0  -- Извлекаем символ с индексом 0.

check :: Maybe Int -> String
check aCoefficient
    | aCoefficient == Nothing = "Invalid string!"  -- Коэффициент пустой!
    | otherwise = show aCoefficient

main :: IO ()
main = print $ check $ coefficientFromString "0"
```

Мы пытаемся извлечь цифру из полученной строки, но что делать, если первым символом этой строки является нечисловой символ? Возвращать `-1`? Это не всегда приемлемо. Поэтому мы возвращаем значение типа `Maybe Int`, которое может содержать в себе извлечённую цифру, а может и не содержать. И уже в функции `check` мы проверяем, с каким конструктором было создано значение `aCoefficient`. Если с конструктором `Nothing` - значит извлечь цифру не удалось.

Обратите внимание: мы явно проверяем аргумент на равенство с нульарным конструктором:

```haskell
aCoefficient == Nothing
```

Однако в модуле `Data.Maybe` есть два удобных предиката, `isJust` и `isNothing`. Поэтому мы могли бы написать так:

```haskell
isNothing aCoefficient
```

## Ещё и монада

Как уже было сказано в предыдущей главе, тип `Maybe` - это не просто опциональная обёртка, это ещё и монада. Поэтому мы можем делать вот такие вещи:

```haskell
import Data.List
import Data.Maybe

result :: Maybe String -> String
result email = if isNothing email then "Bad email" else "Good!"

main :: IO ()
main =
    print $ result $ Just "me@gmail.com" >>= checkFormat >>= checkDomain
    where checkFormat email =
              if '@' `elem` email then return email else Nothing
          checkDomain email =
              if ".com" `isSuffixOf` email then return email else Nothing
```

Обмотали почтовый адрес в `Maybe`, прогнали его через проверочный конвейер и сделали вывод о корректности адреса. Если адрес некорректен - нам неважно, на какой стадии будет обнаружена ошибка, в любом случае конечный вывод будет однозначен.

Ну вот, теперь вы знаете о `Maybe`.

