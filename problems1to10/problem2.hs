myButLast :: [a]  -> a
myButLast [] = error "The list should constain at least 2 elements."
myButLast [butLast, _] = butLast
myButLast (_:tail) = myButLast tail

main = do
    print(myButLast "abc")
    print(myButLast [3, 2])
    print(myButLast [1])
   