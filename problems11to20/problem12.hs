encodeImpl :: (Eq a) => [a] -> Int -> [(Int, a)]
encodeImpl [a] b = [(b+1,a)] 
encodeImpl (x:xs) b = if x == head xs then encodeImpl xs (b+1)  else [(b+1,x)] ++ encodeImpl xs 0 

encode :: (Eq a) => [a] -> [(Int, a)] 
encode xs = encodeImpl xs 0

transform :: (Int, a)-> Item a
transform (1,x)= Single x
transform (n,x)= Multiple n x

data Item a = Single a | Multiple Int a
    deriving (Show) -- extends "Show"

encodeMod :: Eq a => [a] -> [Item a]
encodeMod = map transform . encode

toStringMod :: [Item a] -> [a]
toStringMod = concatMap split -- "concatMap" creates a list from a list generating function by application of this function on all elements in a list passed as the second argument
    where -- declares local functions 
      split (Single x)     = [x]
      split (Multiple n x) = replicate n x

main = do
    print(toStringMod . encodeMod $ "aaaabc")
