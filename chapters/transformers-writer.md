----
title: Трансформеры: писатель
prevChapter: /ru/delicious/transformers-reader.html
nextChapter: /ru/delicious/transformers-state.html
----

От читателя перейдём к писателю. И если вы освоили `ReaderT`, то с `WriterT` у вас не возникнет никаких трудностей. Этот трансформер столь же элегантен и столь же полезен.

## Суть

`WriterT` - это писатель. Он, подобно `ReaderT`, предоставляет неким функциям общее значение, но если читатель предоставляет его для чтения, то писатель, как вы уже догадались, предоставляет его для записи. Одно из канонических применений такого механизма - логирование. Функции работают, записывая некую служебную информацию в "облачный лог". Помните, в предыдущей главе я провёл аналогию между `ReaderT` и облачными технологиями? Писатель тоже создаёт "облако". Рассмотрим пример.

## Пример с URL: добавляем логирование

Помните, в [главе о функциональных цепочках](http://ohaskell.dshevchenko.biz/ru/about-functions/functional-chains.html) мы создали три функции, корректирующие некрасивый URL? Давайте добавим туда лог:

```haskell
import Control.Monad.Writer.Lazy
import Data.Char
import Data.String.Utils

type Environment = String
type LoggingCloud = WriterT Environment IO String

addPrefix :: String -> LoggingCloud
addPrefix url = do
    tell "addPrefix... "
    return $ if url `startsWith` prefix then url else prefix ++ url
    where prefix = "http://"
          startsWith url prefix = startswith prefix url

encodeAllSpaces :: String -> LoggingCloud
encodeAllSpaces url = do
    tell "encodeAllSpaces... "
    return $ replace " " "%20" url

makeItLowerCase :: String -> LoggingCloud
makeItLowerCase url = do
    tell "makeItLowerCase... "
    return $ map toLower url

main :: IO ()
main = do
    let url = "www.SITE.com/test me/Start page"
    result <- runWriterT $ addPrefix url
                           >>= encodeAllSpaces
                           >>= makeItLowerCase
    let correctedUrl = fst result
        log = snd result
    putStrLn $ "Corrected URL: " ++ correctedUrl ++ "\n" ++
               "Log: " ++ log
```

Результат будет таким:

```bash
Corrected URL: http://www.site.com/test%20me/start%20page
Log: addPrefix... encodeAllSpaces... makeItLowerCase...
```

Как видите, никаких радикальных перемен не произошло. Нам потребовалось лишь заменить возвращаемые функциями-корректировщиками значения на наше "монадическое облако". И теперь внутри каждой из этих функций мы можем записывать что-нибудь интересное в это "облако". Для этого используется стандартная функция `tell`:

```haskell
makeItLowerCase :: String -> LoggingCloud
makeItLowerCase url = do
    tell "makeItLowerCase... "  -- Сохраняем в логирующее облако...
    return $ map toLower url    -- ... и туда же кладём наш изменённый URL...
```

Как и в случае с читателем, лог не потребовалось явно передавать от функции к функции в виде аргумента. Работать с "облаком" значительно удобнее.

Как видите, в функции `main` изменения тоже небольшие:

```haskell
main :: IO ()
main = do
    let url = "www.SITE.com/test me/Start page"
    result <- runWriterT $ addPrefix url
                           >>= encodeAllSpaces
                           >>= makeItLowerCase
    let correctedUrl = fst result
        log = snd result
    putStrLn $ "Corrected URL: " ++ correctedUrl ++ "\n" ++
               "Log: " ++ log
```

Функция `runWriterT` запускает наше "облачное" логирование. Значение типа `LoggingCloud` незримо передаётся от корректировщика к корректировщику, за это отвечает наш старый друг, оператор последовательной связки `(>>=)`. А в итоге, на выходе из функции `runWriterT` мы получаем пару, завёрнутую в используемую нами `IO`-обёртку. Вытащив пару в `result`, мы применили к ней функции `fst` и `snd` и получили первый элемент (скорректированный URL) и второй (наш лог).

## А монада Writer?

Как вы уже могли догадаться, монада `Writer` есть простейший вариант трансформера `WriterT`, также как и монада `Reader` есть простейший вариант `ReaderT`. Вот её определение:

```haskell
type Writer w = WriterT w Identity
```

Всё то же самое. Следовательно, мы можем упростить наш пример, отказавшись от внутренней `IO`:

```haskell
type Environment = String
type LoggingCloud = Writer Environment String

-- Остальные функции остаются без изменений...

main :: IO ()
main = do
    let url = "www.SITE.com/test me/Start page"
        result = runWriter $ addPrefix url
                             >>= encodeAllSpaces
                             >>= makeItLowerCase
        correctedUrl = fst result
        log = snd result
    putStrLn $ "Corrected URL: " ++ correctedUrl ++ "\n" ++
               "Log: " ++ log
```

Опять-таки, всё то же самое. Функция `runWriter` возвращает результат уже без `IO`-обёртки, так что код чуток упрощается.

С логированием разобрались, а теперь рассмотрим не менее интересный пример.

## Анализ файла

Представим, что наше приложение собирает информацию о файлах исходного кода. Скажем, есть файл, и нужно узнать используемый в нём язык программирования, дату его последней модификации и его размер.

Для начала определим наш "облачный" тип:

```haskell
import Control.Monad.Writer.Lazy
import System.Directory
import System.FilePath
import System.Posix

type Environment = String
type FileInfoCloud = WriterT Environment IO String
```

Теперь определим три функции-анализатора, каждая из которых получает свою порцию статистической информации и кладёт её в нашем монадическом "облаке". Этим функциям требуется единственный аргумент - путь к файлу. И самое интересное в том, что единственная полезная работа, производимая каждой из этих функций - это запись информации о файле в "облако". То есть если в предыдущем примере функции передавали друг другу постепенно изменяемый URL, а логирование в "облако" было лишь "побочным действием", то здесь запись информации в "облако" и есть главная работа.

А вот и наша первая функция, определяющая используемый в файле язык программирования. Для простоты будем опираться на расширение:

```haskell
obtainLanguage :: FilePath -> FileInfoCloud
obtainLanguage path = do
    tell $ case takeExtension path of
                ".c"   -> "C, "
                ".cpp" -> "C++, "
                ".hs"  -> "Haskell, "
                ".m"   -> "Objective-C, "
                _      -> "Unknown, "
    return path
```

Обратите внимание, что `path`, поступающий на вход, анализируется и затем просто возвращается обратно в монаду. Вся полезная работа заключается только в записи в "облако", с помощью нашей подруги `tell`. Вы спросите, зачем же нам возвращать путь, раз мы всё равно ничего с ним не делаем? А это нужно для формирования нашей любимой монадической цепочки, в функции `main`.

Определим ещё два анализатора:

```haskell
obtainModificationDate :: FilePath -> FileInfoCloud
obtainModificationDate path = do
    date <- liftIO $ getModificationTime path
    tell $ show date ++ ", "
    return path

obtainSize :: FilePath -> FileInfoCloud
obtainSize path = do
    status <- liftIO $ getFileStatus path
    tell $ (show $ fileSize status) ++ " bytes"
    return path
```

А теперь пишем `main`:

```haskell
main :: IO ()
main = do
    let path = "/Users/dshevchenko/main.cpp"
    info <- execWriterT $ obtainLanguage path
                          >>= obtainModificationDate
                          >>= obtainSize
    putStrLn $ "Info about " ++ path ++ ":\n"
               ++ info
```

Вы заметили? Нет никакой пары, и нам больше не нужны функции `fst` и `snd`. И самое главное, исчезла функция `runWriterT`, а её место заняла новая функция `execWriterT`. В ней-то всё и дело. Если `runWriterT` возвращает пару с "облачной" записью и с конечным значением, то `execWriterT` возвращает только "облачную" запись. И это как раз наш случай! Если бы мы использовали функцию `runWriterT`, то опять имели бы дело с парой, первым элементом которой был бы путь к анализируемому файлу. Но зачем запихивать его в финальную пару, если он всё равно никак не изменился? Реальную ценность для нас представляет только "облачная" запись, содержащая собранные данные о файле, поэтому в `info` она и лежит. И вот он результат:

```bash
Info about /Users/dshevchenko/main.cpp:
C++, 2014-03-29 20:59:29 UTC, 1835 bytes
```

## И снова про лифт

Кстати, вы заметили кое-что новенькое в наших анализаторах? Вспомним:

```haskell
obtainModificationDate :: FilePath -> FileInfoCloud
obtainModificationDate path = do
    date <- liftIO $ getModificationTime path  -- Что это за liftIO?
    tell $ show date ++ ", "
    return path
```

Функция `getModificationTime` возвращает значение типа `IO UTCTime`, однако сам анализатор `obtainModificationDate` имеет дело с более высокоуровневой монадой. Нам на помощь приходит функция `liftIO`, похожая на уже известную нам функцию `lift`. Она берёт значение типа `IO UTCTime` и поднимает его на следующий уровень, то есть на уровень нашего `WriterT`. Затем мы извлекаем из этого поднятого значения дату модификации и записываем её в "облачный журнал".

## В сухом остатке

1. Трансформер `WriterT` полезен тогда, когда группе функций нужен некий общий журнал для записей.
2. Функция `runWriterT` возвращает как результат работы функций, имеющих доступ к журналу, так и сам журнал.
3. Каждая из функций оставляет запись в общем журнале с помощью функции `tell`.
4. Монада `Writer` - это простейший вариант трансформера `WriterT`.
5. Функция `execWriterT` возвращает только журнал, что полезно в тех случаях, когда именно журнал и представляет для нас ценность.
