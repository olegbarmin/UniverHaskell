myLast :: [Int]  -> Int
myLast [singleElement] = singleElement -- The case when list consists of one element
myLast (_:tail) = myLast tail -- For the case when the list consists of two or more elements. In this case, the function recursively calls itself with the same list but without its head.
-- '_' sign means that we dont care about value of the variable.
myLast [] = error "List is empty!" -- The case when the given list is empty. In this case, the error will be thrown.

assertEquals:: [Char] -> Int -> Int -> Bool
assertEquals msg expected actual = if expected == actual
                         then True
                         else error (msg ++ ". Actual: " ++ show actual ++ ", expected: " ++ show expected)

main = do
    print(assertEquals "Should return last element of the list." 3 (myLast [1,2,3]))
    print(assertEquals "Should return last element if list constist of one element." 3 (myLast [3]))
    print("Shoud throw an error: ")
    print(myLast []) 
   