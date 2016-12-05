# Haskell
## src/OrdNub.hs
Группа реализаций, которые используют упорядочивание данных. Все методы имеют сигнатуру
`(Ord a) => [a] -> [a]`.
* ordNub
  * Релизация через `Map`
* ordNub2 
  * `map head . group . sort`
* ordNub3
  * Оптимизация `ordNub2`, которая сразу делает `head . group`
  
## src/HashNub.hs
Реализации на основе хеш-таблиц с сигнатурой `(Hashable a) => [a] -> [a]`.
* hashNub
  * Аналог `ordNub` на `HashMap`
  
## src/StuNub.hs
Реализации на основе STUArray. Для исправления STUArray и поддержки полиморфизма
добавлен модуль UnboxArray с тайп-классом `Unbox`. Все реализации имеют сигнатуру
`(Ord a, Hashable a) => [a] -> [a]`.
* stuNubSort
  * Релизация, которая делает `unique` в массиве на отсортированном списке
* stuNub
  * Аналог `ordNub` на массиве

## Сравнение алгоритмов

Результаты представлены в файле *criterion.html*.