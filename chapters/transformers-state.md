----
title: Трансформеры: состояние
prevChapter: /ru/delicious/transformers-writer.html
nextChapter: /ru/delicious/transformers-error.html
----

Ну вот мы и добрались до состояния. Трансформер `StateT` знаменит не меньше своих собратьев `ReaderT` и `WriterT`, столь же часто используется в реальных проектах, столь же удобен, но при этом обладает дополнительными возможностями.

## Суть

`StateT` - это состояние. Фактически, это `ReaderT` и `WriterT` в одном флаконе, поскольку предоставляет группе функций общее значение, предназначенное как для чтения, так и для изменения. А учитывая, что вы уже умеете работать и с читателем, и с писателем, с состоянием у вас не возникнет ни малейших трудностей. Перейдём к примеру.

## Украшаем отчёт

Возьмём пример со сбором информации о файле и украсим его:

```haskell
import Control.Monad.State.Lazy
import System.Directory 
import System.FilePath 
import System.Posix 

type Environment = String
type FileInfoCloud = StateT Environment IO String

indent = "  "
delimiter = ", \n"

obtainLanguage :: FilePath -> FileInfoCloud
obtainLanguage path = do
    modify (++ indent ++ case takeExtension path of
                             ".c"   -> "C"
                             ".cpp" -> "C++"
                             ".hs"  -> "Haskell"
                             ".m"   -> "Objective-C"
                             _      -> "Unknown")
    modify (++ delimiter)
    return path

obtainModificationDate :: FilePath -> FileInfoCloud
obtainModificationDate path = do
    date <- liftIO $ getModificationTime path
    modify (++ indent ++ show date ++ delimiter)
    return path

obtainSize :: FilePath -> FileInfoCloud
obtainSize path = do
    status <- liftIO $ getFileStatus path
    modify (++ indent ++ (show $ fileSize status) ++ " bytes")
    return path

main :: IO ()
main = do
    let path = "/Users/dshevchenko/main.cpp"
    info <- execStateT (obtainLanguage path 
                        >>= obtainModificationDate
                        >>= obtainSize) 
                       "" -- Пустое "облако" для записи...
    putStrLn $ "Info about " ++ path ++ ":\n"
               ++ info
```

Результат будет таким:

```bash
Info about /Users/dshevchenko/main.cpp:
  C++,
  2014-03-29 20:59:29 UTC,
  1835 bytes 
```

Так гораздо красивее, не правда ли? Сравните с предыдущим вариантом:

```bash
Info about /Users/dshevchenko/main.cpp:
C++, 2014-03-29 20:59:29 UTC, 1835 bytes
```

Теперь разберёмся, как же нам удалось сделать такую красоту. К тому же, как вы успели заметить, мы внесли совсем немного изменений, по сравнению с примером с `WriterT`.

Рассмотрим наш первый анализатор:

```haskell
obtainLanguage :: FilePath -> FileInfoCloud
obtainLanguage path = do
    modify (++ indent ++ case takeExtension path of
                             ".c"   -> "C"
                             ".cpp" -> "C++"
                             ".hs"  -> "Haskell"
                             ".m"   -> "Objective-C"
                             _      -> "Unknown")
    modify (++ delimiter)
    return path
```

Функция `tell` исчезла, а её место заняла новая функция `modify`. Трансформер `StateT` даёт нам три основные функции для работы с общим значением, парящем в "облаке":

* `get` извлекает значение из "облака";
* `modify` изменяет значение в облаке, путём применения к нему некой функции;
* `put` кладёт новое значение в "облако", перезаписывая старое.

Воспользовавшись услугами функции `modify`, мы украсили сохраняемую в "облаке" строку: в начало добавили отступ, а в конец дописали запятую с переводом строки.

Теперь взглянем на нашу обновлённую `main`:

```haskell
main :: IO ()
main = do
    let path = "/Users/dshevchenko/main.cpp"
    info <- execStateT (obtainLanguage path 
                        >>= obtainModificationDate
                        >>= obtainSize) 
                       "" -- Пустое "облако" для записи...
    putStrLn $ "Info about " ++ path ++ ":\n"
               ++ info
```

Тут всё почти так же. `execStateT` запускает "облако" и возвращает собранную анализаторами строку. Правда, "заготовку" для облака мы всё-таки должны предоставить, пусть даже и в виде пустой строки. Впрочем, можно было и так:

```haskell
main :: IO ()
main = do
    let path = "/Users/dshevchenko/main.cpp"
    info <- execStateT (obtainLanguage path 
                        >>= obtainModificationDate
                        >>= obtainSize) 
                       $ "Info about " ++ path ++ ":\n"
    putStrLn info
```

Мы сразу добавили общий префикс к нашей информационной строке, а собираемая анализаторами информация будет постепенно дописываться в конец. Результат будет точно таким же.

Существует три варианта функций запуска работы с трансформером `StateT`:

* `runStateT` возвращает знакомую нам пару, первым элементом которой идёт результат работы функций, а вторым - "облачное" значение;
* `evalStateT` возвращает только результат работы функций;
* `execStateT` возвращает только "облачное" значение.

## И наша монада State

Уверен, вы уже догадались: монада `State` является простейшим вариантом трансформера `StateT`. Вот уже знакомое нам определение:

```haskell
type State s = StateT s Identity
```

Следовательно, если внутренняя монада нам не нужна, `StateT` можно спокойно заменить на `State`. Всё точно так же, как в случае с `Reader` и `Writer`.

## Составное облако

До сих пор в нашем "облаке" висело простое значение. Давайте слегка усложним его, воспользовавшись уже известным нам составным типом. В самом деле, зачем явно передавать путь к файлу в виде аргумента, если можно запихнуть его в наше "облако"? Сделаем это.

Для начала определим информационный тип и обновим "облачный" тип:

```haskell
data FileInfo = FileInfo { path :: FilePath
                         , info :: String
                         }

type Environment = FileInfo
type FileInfoCloud = StateT Environment IO ()
```

Первым полем в составном типе `FileInfo` идёт путь к файлу, вторым - информационная строка. Теперь изменим `main`:

```haskell
main :: IO ()
main = do
    let aPath = "/Users/dshevchenko/main.cpp"
        initInfo = FileInfo { path = aPath
                            , info = "Info about " ++ aPath ++ ":\n"
                            }
    fileInfo <- execStateT (obtainLanguage
                            >> obtainModificationDate
                            >> obtainSize)
                           initInfo  -- Размещаем в нашем "облаке"...
    putStrLn $ info fileInfo
```

Создаём подготовительный информационный объект `initInfo`, сразу поместив в него путь к исследуемому файлу и информационную строку. И когда мы размещаем `initInfo` в нашем "облаке", каждая из функций-анализаторов получает доступ как к пути (первому полю), так и к информационной строке (второму полю).

Изменим наш первый анализатор:

```haskell
obtainLanguage :: FileInfoCloud
obtainLanguage = do
    fileInfo <- get  -- Извлекаем информационный объект из "облака"...
    let currentInfo = info fileInfo  -- Вытащили текущую информацию...
        pathToFile = path fileInfo   -- Вытащили путь к файлу...
        infoWithLanguage = currentInfo 
                           ++ indent 
                           ++ case takeExtension pathToFile of
                                  ".c"   -> "C"
                                  ".cpp" -> "C++"
                                  ".hs"  -> "Haskell"
                                  ".m"   -> "Objective-C"
                                  _      -> "Unknown"
                           ++ delimiter
    put $ fileInfo { info = infoWithLanguage }
```

Здесь задействованы все три базовые функции работы со `StateT`. Сначала мы извлекаем информационный объект из "облака" с помощью функции `get` и получаем его текущие поля. Затем мы создаём строку `infoWithLanguage` с указанием языка программирования. И в завершении обновляем информацию. Обратите внимание на эту строку:

```haskell
   put $ fileInfo { info = infoWithLanguage }
```

Здесь мы воспользовались уже знакомой нам конструкций с фигурными скобками. В результате поле `info` было заменено только что созданной `infoWithLanguage`. И сразу после этого мы поместили обновлённый информационный объект обратно в "облако", с помощью функции `put`.

Кстати, вы заметили ещё одно изменение в `main`? Взгляните вот на эту цепочку:

```haskell
execStateT (obtainLanguage
            >> obtainModificationDate
            >> obtainSize)
           initInfo
```

Оператор `(>>=)` исчез, уступив своё место оператору `(>>)`. А иначе и быть не может. Вспомним наш "облачный" тип:

```
type FileInfoCloud = StateT Environment IO ()
                                        |
                                        Здесь теперь пустышка...
```

Во внутренней `IO`-обёртке теперь ничего нет, ведь от анализатора к анализатору никакое значение уже не передаётся, поскольку всё происходит в "облачном" информационном объекте. Поэтому оператор последовательный связки и был заменён оператором "затем".

Не скажу, что пример с составным облаком получился проще. Да, мы избавились от необходимости явно передавать путь к исследуемому файлу в виде аргумента. Но зато нам потребовался новый составной тип, к тому же пришлось работать не только с `modify`, но и с `get` и с `put`. Главное, что мы убедились: в "облаке" может лежать сложный объект. Кстати, именно этот подход задействован в уже упомянутом пакете `direct-fastcgi`, когда `ReaderT` хранит в себе составной объект с параметрами веб-запроса.

## В сухом остатке

1. Трансформер `StateT` - это `ReaderT` и `WriterT` в одном флаконе.
2. Функции `get`, `modify` и `put` позволяют соответственно получить, изменить и переписать наше "облачное" значение. 
3. Монада `State` - это простейший вариант трансформера `StateT`.
4. Функции `runStateT`, `evalStateT` и `execStateT` используются в зависимости от того, что нас интересует: только результат работы наших функций, только "облачное" значение или же и то и другое.
5. В "облаке", созданном `StateT`, можно расположить составной объект, что позволяет упростить сигнатуры функций, использующих "облачное" значение.

