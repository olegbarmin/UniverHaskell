import System.Random

getRandomElements :: [a] -> Int -> IO[a]
getRandomElements [] _ = return []
getRandomElements _ 0 = return []
getRandomElements list n =  do 
                        r <- randomRIO (0,(length list)-1)
                        let remaining = take r list ++ drop (r+1) list
                        rest <- getRandomElements remaining (n-1)
                        return ((list!!r) : rest)

diffSelect :: Int->Int -> IO[Int]
diffSelect n to | n < to = getRandomElements [1..to] n 
                 | n > to = getRandomElements [1..n] to
                 | n == to = getRandomElements [1..n] n


main = diffSelect 6 49 >>= print