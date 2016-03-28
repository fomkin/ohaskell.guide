# О форматировании

Haskell-код является форматно-зависимым, поэтому мы не можем расставлять отступы там, где нам заблагорассудится. Необходимо придерживаться определённых правил.

## Функция

Если мы напишем так:

```haskell
main :: IO ()
main =
putStrLn "Hi Haskeller!"
```

компилятор выскажет своё несогласие:

```bash
parse error (possibly incorrect indentation or mismatched brackets)
```

Следующий пример:

```haskell
 main :: IO ()
 main =
 putStrLn "Hi Haskeller!"
```

Здесь мы поставили один пробел перед каждой из трёх строк, однако и в этом случае компилятор закапризничает:

```bash
parse error on input `main'
```

Или вот так:

```haskell
main :: IO ()
 main =
 putStrLn "Hi Haskeller!"
```

В этом случае мы получим ещё более странную ошибку:
 
```bash
Illegal type signature: `IO () main'
```

Из-за пробела перед именем функции компилятор принял это имя за часть сигнатуры.

Когда в теле функции несколько строк, появляются дополнительные ограничения. Если напишем так:

```haskell
main :: IO ()
main = do
    putStrLn "Hi Haskeller!"
     putStrLn "Hi again!"
```

получим вот это:
 
```bash
Couldn't match expected type `(String -> IO ()) -> [Char] -> IO ()'
                with actual type `IO ()'
    The function `putStrLn' is applied to three arguments,
    but its type `String -> IO ()' has only one
```

Из-за сдвига второй функции по отношению к первой компилятор подумал, что первая по счёту `putStrLn` применяется к трём аргументам. Если же напишем так:

```haskell
main :: IO ()
main = do
     putStrLn "Hi Haskeller!"
    putStrLn "Hi again!"
```

получим уже знакомую нам ошибку:

```bash
parse error on input `putStrLn'
```

Здесь компилятор ругнулся уже на вторую по счёту `putStrLn`.

В общем, экспериментальным путём я выяснил, что форматирование кода функции должно соответствовать следующим правилам:

1.  Объявление и определение функции, должны начинаться с первого (самого левого) символа строки.
2.  Если тело функции начинается со следующей строки после имени, перед этим телом должен присутствовать отступ от первого символа строки, хотя бы в один пробел.
3.  Если тело функции состоит из нескольких выражений, стоящих на отдельной строке каждая, эти выражения должны быть вертикально выровнены по левому краю.

Поэтому придерживайтесь приблизительно такого шаблона:

```haskell
main :: IO ()
main = do
    putStrLn "Hi Haskeller!"
    putStrLn "Hi again!"
```

и компилятор будет просто счастлив.

## Тип

На код, связанный с типами, также наложены некоторые форматные ограничения.

```haskell
 data IPAddress = IP String
```

Перед словом `data` стоит лишний пробел, и компилятор вновь вспоминает нас недобрым словом:

```haskell
parse error on input `data'
```

Вот такой код тоже не пройдёт компиляцию:

```haskell
data
IPAddress = IP String
```

равно как и такой:

```haskell
data IPAddress =
IP String
```

и даже такой:

```haskell
data IPAddress
= IP String
```

В ходе экспериментов было выяснено, что правила для кода определения типа схожи с вышеупомянутыми правилами для кода функции:

1.  Ключевое слово `data` начинается с самого левого символа строки.
2.  Если объявление переходит на следующую строку, то перед ним должен быть хотя бы один пробел.

Поэтому пишите приблизительно так:

```haskell
data IPAddress = IP String
                 deriving Show
```

и компилятор будет вам благодарен.

## Класс типов

С классами типов - та же история. Если напишем так:

```haskell
class Note n where
write :: n -> Bool
```

получим экзотическую ошибку:

```bash
The type signature for `write' lacks an accompanying binding
```

Если вздумаем написать так:

```haskell
class Note n where
  write :: n -> Bool
 read :: n -> String
```

снова получим по башке:

```bash
parse error on input `read'
```

И если так напишем:

```haskell
class Note n where
    write :: n -> Bool
    read
    :: n -> String
```

и даже если так:

```haskell
  class Note n where
    write :: n -> Bool
    read :: n -> String
```

компилятор будет принципиален до крайности и не пропустит такой код.

В общем, тут правила точно такие же:

1.  Начинаем с самого левого символа строки.
2.  Перед методами - хотя бы однопробельный отступ.
3.  Методы должны быть вертикально выровнены по левому краю.

Следовательно, ублажаем компилятор и пишем примерно так:

```haskell
class Note n where
    write :: n -> Bool
    read :: n -> String
```

## Константа

Для отдельной константы правила точно такие же, как и для функции. Поэтому пишем:

```haskell
coefficient :: Double
coefficient = 0.0036
```

и всё будет хорошо.

## Условие

Тут я выявил лишь одно ограничение - край ключевого слова `if` должен быть самым левым по отношению ко всем остальным частям выражения. То есть можно написать так:

```haskell
main :: IO ()
main = do
    if 2 /= 2
      then
        putStrLn "Impossible"
      else
        putStrLn "I believe"
```

и так:

```haskell
main :: IO ()
main = do
    if 2 /= 2
        then
      putStrLn "Impossible"
        else
      putStrLn "I believe"
```

и даже так:

```haskell
main :: IO ()
main = do
    if 2 /= 2

     then
     putStrLn "Impossible"
     else
          putStrLn "I believe"
```

Но вот такого компилятор не потерпит:

```haskell
main :: IO ()
main = do
    if 2 /= 2   
   then
      putStrLn "Impossible"
   else
      putStrLn "I believe"
```

равно как и такого:

```haskell
main :: IO ()
main = do
    if 2 /= 2   
    then
    putStrLn "Impossible"
    else
    putStrLn "I believe"
```

## Локальные выражения

Эти друзья менее прихотливы. В отношении выражения `where` я нашёл только одно ограничение:

```haskell
prepare :: String -> String
prepare str =
    str ++ helper
where
    helper = "dear. "
```

В этом случае получим ошибку:

```bash
parse error on input `where'
```

Такого же рода ограничение действует и на `let`:

```haskell
prepare :: String -> String
prepare str =
let helper = "dear. "
    in
    str ++ helper
```

Однако ошибка будет другой:

```bash
parse error (possibly incorrect indentation or mismatched brackets)
```

Суть вы уловили: пусть `where` и `let` гармонируют с остальным кодом тела функции.

## Вывод

Пишите аккуратно, придерживаясь единого стиля без лишних изысков.
