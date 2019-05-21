import System.Random

removeItem :: Int -> [a] -> [a]
removeItem i x = take (i-1) x ++ drop(i) x

randomPermutation :: [a]->IO [a]
randomPermutation []  = return []
randomPermutation list  = do  
                    r <- randomRIO(0,(length list)-1)
                    rest <- randomPermutation (removeItem (r+1) list)
                    return ((list!!r):rest)

main = do
    randomPermutation "abcdef" >>= print