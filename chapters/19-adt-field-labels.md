# АТД: поля с метками

Многие типы в реальных проектах довольно велики. Взгляните:

```haskell
data Arguments = Arguments Port
                           Endpoint
                           RedirectData
                           FilePath
                           FilePath
                           Bool
                           FilePath
```

Значение типа `Arguments` хранит в своих полях некоторые значения, извлечённые из параметров командной строки, с которыми запущена одна из моих программ. И всё бы хорошо, но работать с таким типом абсолютно неудобно. Он содержит семь полей, и паттерн матчинг был бы слишком громоздким, представьте себе:

```haskell
...
  where
    Arguments _ _ _ redirectLib _ _ xpi = arguments
```

Более того, когда мы смотрим на определение типа, назначение его полей остаётся тайной за семью печатями. Видите предпоследнее поле? Оно имеет тип `Bool` и, понятное дело, отражает какой-то флаг. Но что это за флаг, читатель не представляет. К счастью, существует способ, спасающих нас от обеих этих проблем.

## Метки

Мы можем снабдить наши поля метками (англ. label). Вот как это выглядит:

```haskell
data Arguments = Arguments { runWDServer    :: Port
                           , withWDServer   :: Endpoint
                           , redirect       :: RedirectData
                           , redirectLib    :: FilePath
                           , screenshotsDir :: FilePath
                           , noScreenshots  :: Bool
                           , harWithXPI     :: FilePath
                           }
```

Теперь назначение меток куда понятнее. Схема определения такова:

```haskell
data Arguments = Arguments   { runWDServer :: Port }

тип  такой-то    конструктор   метка поля     тип
                                              поля
```

Теперь поле имеет не только тип, но и название, что и делает наше определение значительно более читабельным. Поля в этом случае разделены запятыми и заключены в фигурные скобки.

Если подряд идут два или более поля одного типа, его можно указать лишь для последней из меток. Так, если у нас есть вот такой тип:

```haskell
data Patient = Patient { firstName :: String
                       , lastName  :: String
                       , email     :: String
                       }
```

его определение можно чуток упростить и написать так:

```haskell
data Patient = Patient { firstName
                       , lastName
                       , email     :: String
                       }
```

Раз тип всех трёх полей одинаков, мы указываем его лишь для последней из меток. Ещё пример полной формы:

```haskell
data Patient = Patient { firstName    :: String
                       , lastName     :: String
                       , email        :: String
                       , age          :: Int
                       , diseaseId    :: Int
                       , isIndoor     :: Bool
                       , hasInsurance :: Bool
                       }
```

и тут же упрощаем:

```haskell
data Patient = Patient { firstName
                       , lastName
                       , email        :: String
                       , age
                       , diseaseId    :: Int
                       , isIndoor
                       , hasInsurance :: Bool
                       }
```

Поля `firstName`, `lastName` и `email` имеют тип `String`, поля `age` и `diseaseId` &mdash; тип `Int`, и оставшиеся два поля &mdash; тип `Bool`.

## Getter и Setter?

Что же представляют собой метки? Фактически, это особые функции, сгенерированные автоматически. Эти функции имеют три предназначения: создавать, извлекать и изменять. Да, я не оговорился, изменять. Но об этом чуть позже, путь будет маленькая интрига.

Вот как мы создаём значение типа `Patient`

```haskell
main :: IO ()
main = print $ diseaseId patient
  where
    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }
```

Метки полей используются как своего рода setter (от англ. set, &laquo;устанавливать&raquo;):

```haskell
patient = Patient { firstName    =      "John"
в этом    типа      поле с
значении  Patient   этой меткой  равно  этой строке
```

Кроме того, метку можно использовать и как getter (от англ. get, &laquo;получать&raquo;):

```haskell
main = print $ diseaseId  patient

               метка как  аргумент
               функция
```

Мы применяем метку к значению типа `Patient` и получаем значение соответствующего данной метке поля. Поэтому для получения значений полей нам уже не нужен паттерн матчинг.

Но что же за интригу я приготовил под конец? Выше я упомянул, что метки используются не только для задания значений полей и для их извлечения, но и для изменения. Вот что я имел в виду:

```haskell
main :: IO ()
main = print $ email patientWithChangedEmail
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"  -- Изменяем???
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }
```

При запуске программы получим:

```haskell
j.d@gmail.com
```

Но постойте, что же тут произошло? Ведь в Haskell, как мы знаем, нет оператора присваивания, однако значение поля с меткой `email` поменялось. Помню, когда я впервые увидел подобный пример, то очень удивился, мол, уж не ввели ли меня в заблуждение по поводу неизменности значений в Haskell?!

Нет, не ввели. Подобная запись:

```haskell
patientWithChangedEmail = patient {
  email = "j.d@gmail.com"
}
```

действительно похожа на изменение поля через присваивание ему нового значения, но в действительности никакого изменения не произошло. Когда я назвал метку setter-ом, я немного слукавил, ведь классический setter из мира ООП был бы невозможен в Haskell. Посмотрим ещё раз внимательнее:

```haskell
...
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"  -- Изменяем???
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }
```

Взгляните, ведь у нас теперь два значения типа `Patient`, `patient` и `patientWithChangedEmail`. Эти значения не имеют друг ко другу ни малейшего отношения. Вспомните, как я говорил, что в Haskell нельзя изменить имеющееся значение, а можно лишь создать на основе имеющегося новое значение. Это именно то, что здесь произошло: мы взяли имеющееся значение `patient` и на его основе создали уже новое значение `patientWithChangedEmail`, значение поля `email` в котором теперь другое. Понятно, что поле `email` в значении `patient` осталось неизменным.

Будьте внимательны при инициализации значения с полями: вы обязаны предоставить значения для всех полей. Если вы напишете так:

```haskell
main :: IO ()
main = print $ email patientWithChangedEmail
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"  -- Изменяем???
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
    }

    -- Поле hasInsurance забыли!
```

код скомпилируется, но внимательный компилятор предупредит вас о проблеме:

```haskell
Fields of ‘Patient’ not initialised: hasInsurance
```

Пожалуйста, не пренебрегайте подобным предупреждением, ведь если вы проигнорируете его и затем попытаетесь обратиться к неинициализированному полю:

```haskell
main = print $ hasInsurance patient
  ...
```

ваша программа аварийно завершится на этапе выполнения с ожидаемой ошибкой:

```bash
Missing field in record construction hasInsurance
```

Не забывайте: компилятор &mdash; ваш добрый друг.

## Без меток

Помните, что метки полей &mdash; это синтаксический сахар (англ. syntactic sugar), и мы можем обойтись без него. Даже если тип был определён с метками, как наш `Patient`, мы можем работать с ним по-старинке:

```haskell
data Patient = Patient { firstName    :: String
                       , lastName     :: String
                       , email        :: String
                       , age          :: Int
                       , diseaseId    :: Int
                       , isIndoor     :: Bool
                       , hasInsurance :: Bool
                       }

main :: IO ()
main = print $ hasInsurance patient
  where
    -- Создаём по-старинке...
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      True
                      True
```

Соответственно, извлекать значения полей тоже можно по-старинке, через паттерн матчинг:

```haskell
main :: IO ()
main = print insurance
  where
    -- Жутко неудобно, но если желаете...
    Patient _ _ _ _ _ _ insurance = patient
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      True
                      True
```

С понятием &laquo;синтаксический сахар&raquo; мы встретимся ещё не раз, на куда более продвинутых примерах.

