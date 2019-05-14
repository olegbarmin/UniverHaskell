data Item a = Single a | Multiple Int a deriving Show

-- Factory function
makeItem :: Int -> a -> Item a
makeItem 1 a = Single a
makeItem n a = Multiple n a

encodeDirect :: Eq a => [a] -> [Item a]
encodeDirect [] = []
encodeDirect (x:xs) = compose 1 x xs
    where compose n x [] = [makeItem n x]
          compose n x (y:ys) = if y == x then compose (succ n) y ys else makeItem n x : compose 1 y ys
          --"succ" returns following item in an enumeration (succ 5 --> 6)

main = do
    print(encodeDirect "aaaabccaadeeee")