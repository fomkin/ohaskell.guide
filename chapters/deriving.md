----
title: Наследуемые типы
prevChapter: /ru/about-user-types/composite-types.html
nextChapter: /ru/about-user-types/own-type-classes.html
----

Как вы помните, вывести на экран значение нашего типа `IPAddress` мы смогли лишь после того, как определили собственный экземпляр класса типов `Show`. Однако существует ещё один способ обеспечить "печатаемость" нашего `IPAddress`.

## Наследуем

На сцену выходит ключевое слово `deriving`. Перепишем определение нашего типа:

```haskell
data IPAddress = IP String
                 deriving Show
```

Всё. Мы можем сразу напечатать наше значение:

```haskell
main = print $ IP "127.0.0.1"
```

Вывод:

```bash
IP "127.0.0.1"
```

Готово. Никаких экземпляров. Мы просто определили наш тип как наследуемый от класса `Show`. Именно поэтому нам не нужно определять собственную версию метода `show`, ведь компилятор уже сделал это за нас.

Но вас, вероятно, интересует, откуда же компилятор узнал, _как_ следует выводить на экран значение нашего типа? А он этого и не узнал, поэтому пошёл по пути наименьшего сопротивления. Обратите внимание на вывод:

```haskell
IP "127.0.0.1"
```

Фактически, наш объект явно стрингифицировался в том же виде, в каком и был создан.

Вспомним наш составной тип и сделаем его "печатаемым":

```haskell
data User = User { firstName
                 , lastName
                 , email
                 , yearOfBirth :: String
                 , account
                 , uid :: Integer
                 } deriving Show

main =
    print user
    where user = User { firstName   = "Denis"
                      , lastName    = "Shevchenko"
                      , email       = "me@dshevchenko.biz"
                      , yearOfBirth = "1981"
                      , account     = 1234567890
                      , uid         = 123
                      }
```

Вывод будет таким:

```bash
User {firstName = "Denis", lastName = "Shevchenko", email = "me@dshevchenko.biz", yearOfBirth = "1981", account = 1234567890, uid = 123}
```

Прямая стрингификация: как создали - так и получили.

Кстати, наследоваться можно только от некоторых классов. В соответствии со стандартом Haskell 2010 к таковым относятся:

1. `Eq`,
2. `Ord`,
3. `Enum`,
4. `Bounded`,
5. `Read`,
6. `Show`.

Рассмотрим, что сделает с нашим типом наследование от этих классов.

## Eq и Ord

Наследование от этих двух классов позволит нам сравнивать объекты нашего типа на равенство, а также по признаку больше/меньше. То есть к объекту нашего типа можно будет применять следующие стандартные функции: `==`, `/=`, `compare`, `<`, `<=`, `>`, `>=`, `max`, `min`.

Эти классы - братья-близнецы: если наследуетесь от одного, то скорее всего нужно и от второго. Да, я ведь не сказал: вы можете наследоваться от нескольких классов одновременно. В этом случае они перечисляются в виде кортежа:

```haskell
data IPAddress = IP String
                 deriving (Eq, Ord)
```

## Enum

Наследование от `Enum` сделает объекты нашего типа перечисляемыми. Однако этот тип должен иметь только нульарные конструкторы.

Вспомним наш "протокольный" тип и описательную функцию к нему:

```haskell
data TransportLayer = TCP | UDP | SCTP | DCCP | SPX

descriptionOf :: TransportLayer -> String
descriptionOf protocol =
    case protocol of
        TCP  -> "Transmission Control Protocol"
        UDP  -> "User Datagram Protocol"
        SCTP -> "Stream Control Transmission Protocol"
        DCCP -> "Datagram Congestion Control Protocol"
        SPX  -> "Sequenced Packet Exchange"
```

Поработаем со списком протоколов:

```haskell
main = print [descriptionOf protocol | protocol <- [TCP, UDP]]
```

Вывод:

```bash
["Transmission Control Protocol","User Datagram Protocol"]
```

Здесь мы воспользовались услугами нашего старого друга, генератора списков, чтобы пройтись по всем элементам списка протоколов и вернуть список с соответствующими описаниями.

Но что мы будем делать, если захотим получить описание всех протоколов транспортного уровня? Нам придётся вручную указывать все пять. Ничего страшного в этом нет, однако если бы это были протоколы физического уровня, то их было бы уже порядка двадцати. Писать их вручную - скучно. Но есть у нас один инструмент, позволяющий создать список малыми усилиями. Речь идёт о диапазонах. Вот тут-то и выходит на сцену класс `Enum`.

Наследуем от него наш тип:

```haskell
data TransportLayer = TCP | UDP | SCTP | DCCP | SPX
                      deriving Enum
```

и теперь мы можем использовать его так:

```haskell
main = print [descriptionOf protocol | protocol <- [TCP ..]]
```

Здесь мы использовали бесконечный диапазон, указав лишь первый из протоколов. В результате наш список включит в себя все имеющиеся значения типа `TransportLayer`, и вывод будет таким:

```bash
["Transmission Control Protocol","User Datagram Protocol","Stream Control Transmission Protocol","Datagram Congestion Control Protocol","Sequenced Packet Exchange"]
```

Обращаю ваше внимание на маленькую деталь:

```haskell
[TCP ..]
```

Видите пробел между именем протокола и двумя точками? Он обязателен. Если уберёте его - компилятор выразит своё несогласие.

## Bounded

Когда мы наследуем наш тип от класса `Bounded`, мы получаем возможность применять к нашему типу две стандартные функции, `minBound` и `maxBound`. Обратите внимание: эти функции применяются именно к типу, а не к значению, и возвращают они минимальное и максимальное значение данного типа. Например:

```haskell
main = putStrLn $ "minimal Int value: " ++ show (minBound :: Int)
                  ++
                  ", maximum Int value: " ++ show (maxBound :: Int)
```

Вывод будет таким:

```bash
"minimal Int value: -9223372036854775808, maximum Int value: 9223372036854775807"
```

Мы можем применять эти две функции и к нашим собственным типам. Сделаем же это с протоколами:

```haskell
data TransportLayer = TCP | UDP | SCTP | DCCP | SPX
                      deriving (Show, Enum, Bounded)

main = putStrLn $ "first protocol: " ++ show (minBound :: TransportLayer)
                  ++
                  ", last protocol: " ++ show (maxBound :: TransportLayer)
```

Вывод:

```bash
"first protocol: TCP, last protocol: SPX"
```

Мы применили эти функции к нашему перечисляемому типу, и они вернули, соответственно, "наименьшее" (первое по счёту) и "наибольшее" (последнее по счёту) значения этого типа.

## Read и Show

Эти два класса наделяют значение нашего типа диаметрально противоположными способностями. `Show`, как вы уже знаете, позволяет представлять значение в виде строки, а `Read`, напротив, позволяет извлекать объект из строки. Ничего не напоминает? Да это ведь сериализация: `Show` даёт возможность сериализовать объект в строку, а `Read` - десериализовать его из этой строки. Например:

```haskell
data User = User { firstName
                 , lastName
                 , email
                 , yearOfBirth :: String
                 , account
                 , uid :: Integer
                 } deriving (Show, Read, Eq)

main =
    let object             = user
        serializedObject   = show object
        deserializedObject = read serializedObject
    in print $ object == deserializedObject -- Не сомневайтесь, объекты равны.
    where user = User { firstName = "Denis"
                      , lastName = "Shevchenko"
                      , email = "me@dshevchenko.biz"
                      , yearOfBirth = "1981"
                      , account = 1234567890
                      , uid = 123
                      }
```

Ну вот, теперь вы знаете, что такое `deriving`. Открою вам секрет: наследоваться можно не только от шести вышеперечисленных классов, но и от нескольких других. Однако это касается довольно-таки продвинутых случаев, поэтому сейчас мы не будем их рассматривать.

## В сухом остатке

1. Наследуя тип от некоторого класса, мы добавляем в этот тип черты данного класса.
2. Наследники классов `Eq` и `Ord` могут быть сравнены на (не)равенство. 
3. Наследники класса `Enum` становятся перечисляемыми, что удобно при использовании диапазонов.
4. Наследники класса `Bounded` получают свой "минимум" и "максимум".
5. Наследники классов `Show` и `Read` могут быть сериализованы и десериализованы. 


