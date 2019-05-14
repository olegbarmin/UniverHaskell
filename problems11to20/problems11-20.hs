
----- 1 -----
lastElement :: [a] -> a  -- определение функции
lastElement [x] = x --базовый случай, когда в списке один элемент
lastElement (_:xs) = lastElement xs --пошагово убираем "голову", пока не останется в списке один элемент
lastElement [] = error "An empty list!"

-----2-----
lastButOne :: [a] -> a
lastButOne [] = error "An empty list!" -- пустой список
lastButOne [_] = error "Only one element in a list" -- список из одного элемента
lastButOne [x,_] = x -- возвращает элемент, если после него есть один
lastButOne (_:xs) = lastButOne xs -- убираем по элементу с головы

-----3-----
--(!!) :: [a] -> Int -> a
--(x:_)  !! 0 =  x
--(_:xs) !! n =  xs !! (n-1)


getElementByIndex :: [a] -> Int -> a --список и индекс, возвращает элемент списка
getElementByIndex (x:_)  1 = x --начинаем нумеровать с 1 
getElementByIndex (_:xs) k  -- отрезаем голову и индекс меняет на 1 
                    | k < 1  = error "Index out of bounds"
                    | otherwise = getElementByIndex xs (k-1)


-----4-----

getLength :: [a] -> Int
getLength [] = 0 --если пустой список
getLength (_:xs) = 1 + getLength xs --если список не пустой, добавляем 1, обрезаем голову и рекурсивно повторяем до пустого


-----5-----
reverseList :: [a] -> [a]
reverseList [] = []  -- пустой список остаётся пустым
reverseList (x : xs) = reverseList xs ++ [x] -- конкатенируем последний элемент с хвостом и рекурсивно вызываем

-----6-----
isPalindrome :: (Eq a) => [a] -> Bool -- eq для использования ==
isPalindrome [] = True -- пустой список полиндром
isPalindrome [_] = True --список из одного элемента - полиндром
isPalindrome xs = xs == (reverseList xs) -- если равен перевёрнутому себе, значит полиндром

-----7-----
data NestedList a = Elem a | List [NestedList a] -- новый тип переменной - вложенные списки
toOneList :: NestedList a -> [a] -- передаём вложенный список, получаем список на выходе
toOneList (Elem a) = [a] -- один элемент в список 
toOneList (List (x:xs)) = toOneList x ++ toOneList (List xs) -- отрезаем голову и приводим его к списку, конкатерируем с хвостом, с которым делаем тоже самое
toOneList (List[]) = [] -- список из пустого элемента - просто список


----------8---------
deleteDuplicates :: Eq a => [a] -> [a] -- for using ==
deleteDuplicates (x:ys@(y:_)) -- set second element as a head of a new list
    | x == y    = deleteDuplicates ys -- if first two elements are equal repeat with tail of initial list
    | otherwise = x : deleteDuplicates ys -- append x as a head of list 
deleteDuplicates ys = ys

----------9----------
makeSublists :: (Eq a) => [a] -> [[a]]
makeSublists [] = [] -- empty list
makeSublists (y:ys) = impl ys [[y]] -- create sublist with dublicate element
    where
		impl [] makeSublists = makeSublists
		impl (x:xs) makeSublists 
			| x == (head (last makeSublists)) = impl xs ((init makeSublists) ++ [x:(last makeSublists)]) -- if head == second element create sublist with this element and concatenate with the next one 
			| otherwise		    = impl xs (makeSublists ++ [[x]]) -- make a sublist with one element and, repeat with another part


-----------10---------
countElements ::(Eq a ) => [a] -> [(Int, a)]
countElements [] = []
countElements (x:xs) =  [(getLength x, head x) | x <- makeSublists xs] -- use result of getLenght as first element, and head of sublist as second


-----------additional-------
transform :: (Int, a)-> Item a
transform (1,x)= Single x
transform (n,x)= Multiple n x

------------11--------
----Modify the result of problem 10 in such a way
---that if an element has no duplicates it is simply copied into the result list. 
---Only elements with duplicates are transferred as (N E) lists.
data Item a = Single a | Multiple Int a
    deriving (Show)

countElementsMod :: Eq a => [a] -> [Item a]
countElementsMod = map transform . countElements
--------------12-----------
----Given a run-length code list generated as specified in problem 11. 
---Construct its uncompressed version.
toStringMod :: [Item a] -> [a]
toStringMod = concatMap split
    where
      split (Single x)     = [x]
      split (Multiple n x) = replicate n x

------------13-------------
---Implement the so-called run-length encoding data compression method directly. 
---I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. 
---As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.



-----------------14-------------
--- Duplicate the elements of a list.
dublicateOwn :: Eq a => [a] -> [a]
dublicateOwn [] = []
dublicateOwn (x: xs) = x:x: dublicateOwn xs



---------------15--------------
---Replicate the elements of a list a given number of times.

replicateOwn :: [a] -> Int -> [a]
replicateOwn xs n = concatMap (replicate n) xs



-----------16---------
---Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = drop xs n
    where drop [] _ = []
          drop (x:xs) 1 = drop xs n
          drop (x:xs) k = x : drop xs (k-1)


------------17--------
---Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> ([a], [a])
split (x:xs) n | n > 0 = 
	let (f,l) = 
		split xs (n-1) in (x : f, l)
split xs _ = ([], xs)


-----------18-----------
---Given two indices, i and k, 
---the slice is the list containing the elements 
--between the i'th and k'th element of the original list (both limits included). 
--Start counting the elements with 1.

slice :: [a] -> Int -> Int -> [a]
slice [] _ _  = []
slice (x:xs) i k
 | i > 1      = slice xs (i - 1) (k - 1)
 | k < 1      = []
 | otherwise  = x:slice xs (i - 1) (k - 1)


----------19----------
--- Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate x y
  | y > 0 = rotate (tail x ++ [head x]) (y-1)
  | otherwise = rotate (last x : init x) (y+1)

--------20--------
--Remove the K'th element from a list.
removeAt :: Int -> [a] -> ([a])
removeAt 1 (x:xs) = (xs)
removeAt n (x:xs) = (x:r)
	where r = removeAt (n - 1) xs

---------21---------
--Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs





