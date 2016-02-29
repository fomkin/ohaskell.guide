----
title: Про апостроф
prevChapter: /ru/recursive-functions.html
nextChapter: /ru/about-formatting.html
----

В Haskell частью имени любой программной сущности может выступать апостроф. Да-да, та самая одинарная кавычка, в которые мы помещаем отдельный символ типа `Char`.

Можно включать один или более апострофов в имя функции:

```haskell
strangeFunction' :: Int -> Int
strangeFunction' arg = arg
```

в имя типа:

```haskell
data Strange_'type' = Strange_'type' String
```

в имя класса типов:
 
```haskell
class Stran''geClass'' a where
    fmethod :: a -> String
```

и даже в имя значения:
 
```haskell
strangeValue''' :: Integer
strangeValue''' = 123
```

Во многих Haskell-проектах я встречал апостроф как признак "промежуточного" значения, например:

```haskell
    let path  = "/usr/local"
        path' = path </> "lib"
    in print path'
```

Идею вы поняли: апостроф - легальный символ в любом идентификаторе. Впрочем, излишне говорить, что увлекаться этим символом не рекомендуется.
