myButLast :: [Int]  -> Int
myButLast [butLast, _] = butLast
myButLast (_:tail) = myButLast tail
myButLast [] = error "The list should constain at least 2 elements."

assertEquals:: [Char] -> Int -> Int -> Bool
assertEquals msg expected actual = if expected == actual
                         then True
                         else error (msg ++ ". Actual: " ++ show actual ++ ", expected: " ++ show expected)

main = do
    print(assertEquals "Should return last bu one element of the list." 'b' (myButLast ['a','b','c']))
    print(assertEquals "Should return last but one element of the list of two elements." 3 (myButLast [3, 2]))
    print("Shoud throw an error: ")
    print(myButLast [1])
   