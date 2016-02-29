----
title: Собственные исключения
prevChapter: /ru/io/exceptions-handling.html
nextChapter: /ru/delicious/index.html
----

До сих пор мы лишь ловили исключения. Теперь поговорим о том, как их бросать, а также о том, как создать свои собственные типы исключений.

## Создаём

Определяем тип исключения:

```haskell
import Control.Exception
import Data.String.Utils
import Data.Typeable

type Repo = String

data InvalidRepository = InvalidRepository Repo
                         deriving (Show, Typeable)

instance Exception InvalidRepository
```

Подразумевается, что мы анализируем некий репозиторий, и если он некорректен, в дело вступает исключение `InvalidRepository`. Мы наследовались от двух классов, `Show` и `Typeable`. Сделать это необходимо, потому что наш тип обязан предоставить свой экземпляр класса типов `Exception`, а этот класс устанавливает контекст из этих двух классов.

Вы спросите, что это за необычная строка:

```haskell
instance Exception InvalidRepository
```

Перед нами - экземпляр класса типов `Exception`, но в этом экземпляре нет ни ключевого слова `where`, ни последующих реализаций соответствующих методов. Класс `Exception` содержит в себе два метода, но мы говорим: "Вот наш экземпляр класса типов `Exception`, но предоставлять реализации его методов мы не хотим".

## Бросаем

Чтобы бросить исключение, используем стандартную функцию `throw`. Не забывайте, что даже в том случае, если исключение было брошено из чистой функции, поймать его мы сможем только в `IO`-функции.

Напишем:

```haskell
extractProtocol :: String -> String
extractProtocol path =
    if path `startsWith` "git" || path `startsWith` "ssh"
    then takeWhile (/= ':') path
    else throw $ InvalidRepository path  -- Протокол неверный, кидаем...
    where startsWith url prefix = startswith prefix url

main :: IO ()
main = do
    result <- try $ evaluate $ extractProtocol "ss://ul@sch/proj.git"
                    :: IO (Either SomeException String)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right protocol -> putStrLn protocol
```

Вывод будет таким:

```bash
Fault: InvalidRepository "ss://ul@sch/proj.git"
```

Мы пытаемся извлечь протокол из полного пути к репозиторию, и если там не то, что нам нужно, мы кидаем исключение, перехватываемое функцией `try`.

## В сухом остатке

1. Тип собственного исключения обязан иметь отношение к классу `Exception`.
2. Бросаем исключение функцией `throw`.
3. Ловим исключение как обычно, функцией `try`.

