myLast :: [a] -> a -- объявляем функцию,
myLast [] = error "List is empty!" -- Случай если полученный список пустой: вызываем ошибку с заданным сообщением.
myLast [singleElement] = singleElement -- Случай когда в полученном списке один елемент: возращаем единственный елемент.
myLast (_:tail) = myLast tail -- С помощью шаблонна для списков, определяем что данная функция будет вызыватся для всех случаев когда 
-- в списке будеи более 1 елемента. В этом случаем она будет рекурсивно вызвать себя до тех пор пока в списке не останется один елемент, 
-- который и будет возвращен в итоге.

main = do
    print(myLast [1,2,3])
    print(myLast "abc")
    print(myLast [3])
   