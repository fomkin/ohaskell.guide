----
title: Трансформеры: читатель
prevChapter: /ru/delicious/transformers-first-meeting.html
nextChapter: /ru/delicious/transformers-writer.html
----

В этой главе мы познакомимся с монадным трансформером `ReaderT`. Это весьма любопытный инструмент, часто используемый в реальных проектах. Суть этого трансформера очень проста и элегантна.

## Суть

`ReaderT` - это читатель. Он предоставляет неким функциям общее значение, которое можно прочесть. Такого рода задача возникает достаточно часто. Например, есть некое значение `A`, полученное откуда-то извне (скажем, из конфигурационного файла), и есть набор функций, каждой из которых позарез нужно это значение `A`. Вот `ReaderT` и предоставляет всем этим функциям общий доступ к значению `A`. И чтобы стало понятнее, рассмотрим пример.

## Разбираем git-репозиторий

Пусть нам захотелось покопаться во внутренностях тайного каталога `.git`, незримо присутствующего в наших репозиториях. Для простоты ограничимся выводом всех его внутренних каталогов. Вот как мы можем сделать это обычным способом, без `ReaderT`:

```haskell
gitRoot = "/.git/"

getPathToBranches :: String -> String
getPathToBranches pathToRepo =
    "Branches: " ++ pathToRepo ++ gitRoot ++ "branches"

getPathToHooks :: String -> String
getPathToHooks pathToRepo =
    "Hooks: " ++ pathToRepo ++ gitRoot ++ "hooks"

getPathToLogs :: String -> String
getPathToLogs pathToRepo =
    "Logs: " ++ pathToRepo ++ gitRoot ++ "logs"

getPathToObjects :: String -> String
getPathToObjects pathToRepo =
    "Objects: " ++ pathToRepo ++ gitRoot ++ "objects"

getPathToRefs :: String -> String
getPathToRefs pathToRepo =
    "Refs: " ++ pathToRepo ++ gitRoot ++ "refs"

getPathToInfo :: String -> String
getPathToInfo pathToRepo =
    "Info: " ++ pathToRepo ++ gitRoot ++ "info"

showRepoInternalDirectories :: String -> String
showRepoInternalDirectories pathToRepo =
    let pathToBranches = getPathToBranches pathToRepo
        pathToHooks = getPathToHooks pathToRepo
        pathToLogs = getPathToLogs pathToRepo
        pathToObjects = getPathToObjects pathToRepo
        pathToRefs = getPathToRefs pathToRepo
        pathToInfo = getPathToInfo pathToRepo
    in
    concat [pathToBranches
            ,"\n", pathToHooks 
            ,"\n", pathToLogs 
            ,"\n", pathToObjects 
            ,"\n", pathToRefs 
            ,"\n", pathToInfo]

main :: IO ()
main = do
    pathToRepo <- readFile "/Users/dshevchenko/my.conf"
    let pathWithoutTrailingNL = takeWhile (/= '\n') pathToRepo
        finalInfo = showRepoInternalDirectories pathWithoutTrailingNL
    putStrLn finalInfo
```

Всё просто: забираем строку из файла `my.conf` и передаём её нескольким функциям, каждая из которых указывает путь к соответствующему внутреннему служебному каталогу. Для краткости все проверки файла `my.conf` опущены: такой файл существует, мы можем его прочитать и в нём действительно лежит путь к некоторому репозиторию. Вот результат работы такого кода:

```bash
Branches: /Users/dshevchenko/repo1/.git/branches
Hooks: /Users/dshevchenko/repo1/.git/hooks
Logs: /Users/dshevchenko/repo1/.git/logs
Objects: /Users/dshevchenko/repo1/.git/objects
Refs: /Users/dshevchenko/repo1/.git/refs
Info: /Users/dshevchenko/repo1/.git/info
```

Кстати, мы использовали новую функцию `concat`. Эта стандартная функция принимает список строк и объединяет их в одну. Да, мы могли бы воспользоваться оператором `++`, но с функцией `concat` красивше получилось.

## Ложечка дёгтя

И всё бы хорошо с этим примером, но кое-что цепляет глаз. Получив путь к репозиторию, мы вынуждены явно передавать его в качестве аргумента всем нашим функциям: сначала функции `showRepoInternalDirectories`, а затем всем вспомогательным функциям. Это выглядит громоздко. Тут-то и выходит на сцену наш читатель. Но прежде чем мы рассмотрим решение на его основе, немного теории.

## Совсем немного теории

Вот что представляет собою тип `ReaderT`:

```haskell
newtype ReaderT r m a 
```

Перед нами конструктор типа, параметризованный тремя значениями:
* `r` - общее значение,
* `m` - некая монада,
* `a` - значение внутри этой монады.

Общее значение - это наш главный герой, ради которого всё и затевалось. Иногда его ещё называют "shared environment value". Вы уже догадались, как это относится к нашему примеру, не так ли? Ведь у нас имеется такое общее значение, а именно полученный из конфигурационного файла путь к репозиторию! Раз этот путь нужен всем нашим функциям, мы помещаем его в общую среду ("shared environment"), к которой можно будет получить доступ. В этом и есть суть `ReaderT`: он, грубо говоря, предоставляет глобальную переменную, видимую всем нашим функциям.

Перейдём к ~~магии~~ практике.

## ~~Магия~~ Практика

Определим конкретный тип для нашего случая:

```haskell
import Control.Monad.Reader

type Environment = String
type PathReader = ReaderT Environment IO String
```

Значение типа `Environment` - это и есть наше общее значение, в котором будет храниться путь к репозиторию. А вот теперь начинается немного магии. Перепишем первую вспомогательную функцию, выводящую путь к каталогу `branches`:

```haskell
getPathToBranches :: PathReader
getPathToBranches = do
    pathToRepo <- ask
    return $ "Branches: " ++ pathToRepo ++ gitRoot ++ "branches"
```

Необычно выглядит, не правда ли? Аргумент `pathToRepo` пропал, а возвращается уже не строка, а наш читатель. Интересной здесь является вот эта строка:

```haskell
    pathToRepo <- ask
```

Функция `ask` имеет непосредственное отношение к `ReaderT`. Она запрашивает (англ. "ask") ту самую строку из общей среды читателя. Значение, возвращаемое функцией `ask`, имеет монадический тип `ReaderT Environment`, поэтому идентификатор `pathToRepo` ассоциирован уже с `Environment`, то есть с этой самой строкой. И чтобы работа функции `ask` стала ещё яснее, вспомним получение пользовательской строки:

```haskell
getStringFromUser :: IO String
getStringFromUser = do
    stringFromUser <- getLine
    ...
```

Подобно тому, как функция `getLine` идёт во внешний мир, ассоциированный с монадой `IO`, чтобы принести оттуда введённую пользователем строку, функция `ask` идёт в общую среду, ассоциированную с монадой `ReaderT`, чтобы принести оттуда то самое общее значение.

В этом и заключается маленькое волшебство `ReaderT`: общее значение уже не нужно явно передавать в виде аргумента, поскольку оно как бы незримо парит в воздухе, и теперь все наши функции могут запрашивать его с помощью функции `ask`. И поэтому теперь функция `showRepoInternalDirectories` выглядит так:

```haskell
showRepoInternalDirectories :: PathReader 
showRepoInternalDirectories = do
    pathToBranches <- getPathToBranches
    pathToHooks <- getPathToHooks
    pathToLogs <- getPathToLogs
    pathToObjects <- getPathToObjects
    pathToRefs <- getPathToRefs
    pathToInfo <- getPathToInfo
    return $ concat [pathToBranches
                     ,"\n", pathToHooks 
                     ,"\n", pathToLogs 
                     ,"\n", pathToObjects 
                     ,"\n", pathToRefs 
                     ,"\n", pathToInfo]
```

Аргументов больше нет, есть лишь общая среда.

Но как же наша строка, извлечённая из конфигурационного файла, окажется в той самой общей среде читателя? Пришло время взглянуть на обновлённую `main`:

```haskell
main :: IO ()
main = do
    pathToRepo <- readFile "/Users/dshevchenko/my.conf"
    let pathWithoutTrailingNL = takeWhile (/= '\n') pathToRepo
    finalInfo <- runReaderT showRepoDirectories 
                            pathWithoutTrailingNL
    putStrLn finalInfo
```

Функция `runReaderT` - это волшебная палочка, запускающая (англ. "run") вышеупомянутую магию. Принимая в качестве первого аргумента функцию `showRepoDirectories`, а качестве второго - тот самый путь из конфигурационного файла, функция `runReaderT` создаёт общую среду, с которой и работают все наши функции. Грубо говоря, `runReaderT` создаёт облако и помещает в него значение `pathWithoutTrailingNL`. Вот такие "облачные технологии" внутри Haskell... :-)

## Разоблачение монады Reader

Если вы прочли [главу о первой встрече с трансформерами](http://ohaskell.dshevchenko.biz/ru/delicious/transformers-first-meeting.html), у вас, очевидно, возник вопрос о монаде `Reader`. Ведь, как мы помним, монадный трансформер - это монадическая матрёшка два-в-одном: берём одну монаду и добавляем к ней функциональность другой монады. Мы видели это на примере `MaybeT`: есть монада `Maybe`, а есть трансформер `MaybeT`, привносящий `Maybe`-опциональность в некую другую монаду.

Следовательно, раз есть монадный трансформер `ReaderT`, должна быть и монада `Reader`, не так ли? Но если так, почему же мы сразу взялись за рассмотрение трансформера?

Пришло время узнать правду. А правда в том, что монада `Reader`... это и есть трансформер `ReaderT`. Взглянем на определение в модуле `Control.Monad.Reader`:

```haskell
type Reader r = ReaderT r Identity
```

Вот и вся правда: тип `Reader` есть не более чем псевдоним для `ReaderT`. Таким образом, если монада `Maybe` является самостоятельным типом и ничего не знает о трансформере `MaybeT`, монада `Reader`, напротив, является *частным случаем* трансформера `ReaderT`.

## Зачем нужна Reader

Для простоты. Вы обратили внимание, что тип `Reader` в качестве монады использует тип `Identity`? Это и делает тип `Reader` простейшим вариантом трансформера `ReaderT`, поскольку монада `Identity` - это самая простая стандартная монада из всех существующих, даже проще чем `Maybe`. `Identity` живёт в пакете `transformers` и представляет собою монаду-пустышку. Она настолько примитивна, что сама по себе не имеет никакой ценности. Взгляните:

```haskell
newtype Identity a = Identity a
    deriving (Eq, Ord, Read, Show)

runIdentity :: Identity a -> a
runIdentity (Identity x) = x

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)
```

Грубо говоря, это как `Maybe` с одним лишь конструктором `Just`. Единственное назначение этой монады - быть монадой-пустышкой. Поэтому, когда мы пишем так:

```haskell
type PathReader = ReaderT Environment Identity String
```

или так:

```haskell
type PathReader = Reader Environment String
```

то говорим: "Да, тип `PathReader` по-прежнему содержит в себе значение типа `String`, завёрнутое в монадическую обёртку, но обёртка эта столь примитивна, что её как будто и нет вовсе." Поэтому при использовании `Reader` мы можем вообще не думать о внутренней монаде.

Это позволит нам упростить наш пример и написать так:

```haskell
type Environment = String
type PathReader = Reader Environment String

-- Все остальные функции остаются без изменений...

main :: IO ()
main = do
    pathToRepo <- readFile "/Users/dshevchenko/my.conf"
    let pathWithoutTrailingNL = takeWhile (/= '\n') pathToRepo
        -- runReader сразу вернёт String, а не IO String... 
        finalInfo = runReader showRepoInternalDirectories 
                              pathWithoutTrailingNL
    putStrLn finalInfo
```

В самом деле, зачем нам внутренняя монада `IO` в нашем `PathReader`? В данном случае мы легко можем обойтись и без неё. Так и определение типа `PathReader` упрощается, и в функции `main` мы имеем дело сразу с результирующей строкой, без `IO`-обёртки. Обратите внимание, мы используем уже не `runReaderT`, а `runReader`.

## Реальный пример

Этот небольшой пример основан на использовании прекрасного пакета `direct-fastcgi`, предназначенного для создания FastCGI-скриптов. В центре нашего внимания - обработчик запросов, приходящих к нашему приложению от веб-сервера. Итак:

```haskell
handleRequest :: FastCGI ()
handleRequest = do
    fPutStr "Hello from Haskell world!"

main :: IO ()
main = acceptLoop forkIO handleRequest
```

Функция `handleRequest` обрабатывает поступивший запрос. А вот определение типа `FastCGI`:

```haskell
type FastCGI = ReaderT FastCGIState IO
```

Вот он, наш старый друг! Использование `ReaderT` в данном случае очень удобно: мы получили из внешнего мира запрос и поместили его в наше общее "облако". Функции `handleRequest` обязательно понадобится доступ к параметрам запроса, чтобы адекватно на него ответить. И именно благодаря `ReaderT` работа с этими параметрами очень удобна. Например:

```haskell
handleRequest :: FastCGI ()
handleRequest = do
    queryString <- getQueryString
    setResponseStatus 404
    setResponseHeader HttpContentType "text/plain"
    fPutStr $ "Test: " ++ queryString
```

Все четыре использованные в обработчике функции работают со значением типа `FastCGIState`, но это значение не передаётся им в качестве аргумента. Однако вы уже знаете, в чём здесь секрет: каждая из этих функций извлекает значение типа `FastCGIState` из "общего облака" монады `ReaderT`. В итоге получаем красивый и компактный код.

## В сухом остатке

1. Трансформер `ReaderT` полезен тогда, когда группе функций нужен доступ к некоторому общему значению.
2. Функция `runReaderT` инициализирует "облако" с общим значением и запускает работу с ним.
3. Каждая из функций, используя `ask`, "магически" извлекает общее значение из "облака".
4. Монада `Reader` - это простейший вариант трансформера `ReaderT`.

