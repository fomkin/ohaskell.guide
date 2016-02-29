----
title: Трансформеры: ошибка
prevChapter: /ru/delicious/transformers-state.html
nextChapter: /ru/miscellaneous/index.html
----

В главе, посвящённой [обработке исключений](http://ohaskell.dshevchenko.biz/ru/io/exceptions-handling.html), мы уже говорили о борьбе с ошибками. Но и среди трансформеров есть решения для этой цели. О них и поговорим. Кстати, к этим решениям тоже можно применить аналогию с облачными технологиями.

## ErrorT

Трансформер `ErrorT` чем-то похож на `WriterT`. Он предоставляет группе функций "облако", способное принимать сообщения об ошибках. Причём это "облако" основано на уже известной нам монаде `Either`. Рассмотрим наш первый пример:

```haskell
import Control.Monad.Error

type ErrorMessage = String
type Length = Int
type LengthMonad = ErrorT ErrorMessage IO Length

calculateLength :: LengthMonad
calculateLength = do
    liftIO $ putStrLn "Please enter a non-empty string: "
    stringFromUser <- liftIO getLine
    if null stringFromUser
    then throwError "the string is empty!"  -- Записываем в "облако" ошибку... 
    else return $ length stringFromUser     -- Всё в порядке, записываем длину...

main :: IO ()
main = do
    result <- runErrorT calculateLength
    putStrLn $ case result of
        Left error -> "Hm... Error occured: " ++ error
        Right length -> "The length is " ++ show length
```

Как обычно, `runErrorT` запускает работу функции, в которой может произойти нечто нехорошее. Внутри `calculateLength` мы вновь используем нуш знакомую `liftIO`, чтобы поднять `IO`-монаду на уровень `ErrorT`. Если случилась проблема, мы кидаем исключение с помощью функции `throwError`. Если же всё в порядке, мы записываем в наше "облако" длину введённой строки.

А в функции `main` мы вытаскиваем значение из `ErrorT`, и тип этого значения, как вы уже догадались, `Either ErrorMessage Length`. И потом проверяем, с каким конструктором было создано это значение: если с `Left` - пользователь ввёл пустую строку, если же с `Right` - всё в порядке, получаем длину пользовательской строки.

## EitherT

Трансформер `EitherT` - это брат-близнец `ErrorT`. Однако живёт он в отдельном пакете `either`, не забудьте установить его. Давайте изменим наш пример:

```haskell
import Control.Monad.Trans.Either
import Control.Monad.IO.Class 

type ErrorMessage = String
type Length = Int
type LengthMonad = EitherT ErrorMessage IO Length

calculateLength :: LengthMonad
calculateLength = do
    liftIO $ putStrLn "Please enter a non-empty string: "
    stringFromUser <- liftIO getLine
    if null stringFromUser
    then left "the string is empty!"
    else right $ length stringFromUser

main :: IO ()
main = do
    result <- runEitherT calculateLength
    putStrLn $ case result of
        Left error -> "Hm... Error occured: " ++ error
        Right length -> "The length is " ++ show length
```

Главное изменение произошло в функции `calculateLength`. Взгляните на проверку пользовательской строки:

```haskell
    if null stringFromUser
    then left "the string is empty!"    -- Ошибка, будет в конструкторе Left...
    else right $ length stringFromUser  -- Длина строки, будет в конструкторе Right...
```

Функцию `throwError` заменила функция `left`, а место `return` заняла функция `right`. И это весьма удобно, ведь в `main` мы имеем дело с `Either` и проверяем его конструкторы `Left` и `Right`. Вот и получается, что, сообщая об ошибке, мы явно указываем, что внутри значения `result` сообщение об ошибке окажется *слева* (left), если же всё в порядке, длина строки будет *справа* (right).

Какой из двух близнецов использовать - решать вам.

## Цепочка

Рассмотрим ещё один интересный пример. Путь у нас есть последовательность действий, на каждом из которых может произойти некая ошибка. И если она произошла, нужно немедленно прервать цепочку и сообщить о проблеме. Давайте проанализируем окружение пользователя, а именно наличие у него компиляторов C и C++:

```haskell
import Control.Monad.Trans.Either
import Control.Monad.IO.Class 
import System.Environment 

type ErrorMessage = String
type EnvironmentInfo = EitherT ErrorMessage IO ()

checkCppCompiler :: EnvironmentInfo
checkCppCompiler = do
    -- Анализируем переменную среды...
    maybeCxx <- liftIO $ lookupEnv "CXX"
    case maybeCxx of
        Nothing -> left "CXX compiler not found!"
        Just _ -> right ()  -- Всё в порядке, записываем "пустоту"...

checkCCompiler :: EnvironmentInfo
checkCCompiler = do
    -- И ещё одну переменную среды...
    maybeC <- liftIO $ lookupEnv "CC"
    case maybeC of
        Nothing -> left "CC compiler not found!"
        Just _ -> right ()  -- Всё в порядке, записываем "пустоту"...

main :: IO ()
main = do
    result <- runEitherT $ checkCppCompiler >> checkCCompiler
    putStrLn $ case result of
        Left error -> "Fault: " ++ error
        Right _ -> "OK."
```

Функции `checkCppCompiler` и `checkCCompiler` проверяют наличие переменных среды `"CXX"` и `"CC"` соответственно. Если таковые есть - всё в порядке, и они просто записывают в наше "облако" пустоту. Если же одной из переменных нет, функция, обнаружившая проблему, немедленно сообщает об этом. А из-за того, что функции-анализаторы работают независимо друг от друга, и от первого ничего не переходит ко второму, мы связали их оператором `(>>)`. 

## В сухом остатке

1. `ErrorT` и `EitherT` предназначены для создания "облака", в которое можно бросать сообщения об ошибках.
2. Функции `runErrorT` и `runEitherT` запускают работу с "ошибочным облаком", возвращая в конечном итоге значение типа `Either`.
3. Функция `left` есть аналог `throwError`, а `right` - то же самое что `return`.
4. Функции, использующие "ошибочное облако", удобно использовать для пошагового анализа.

