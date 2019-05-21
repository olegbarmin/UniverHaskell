import System.Random

getRandomElements :: [a] -> Int -> IO[a]
getRandomElements [] _ = return []
getRandomElements _ 0 = return []
getRandomElements list n =  do 
                        r <- randomRIO (0,(length list)-1) -- A variant of 'randomR' that uses the global random number generator (see System.Random).
                        let remaining = take r list ++ drop (r+1) list -- 'remaining' list without random element
                        rest <- getRandomElements remaining (n-1)
                        return ((list!!r) : rest) -- '!!' take element by index

main = getRandomElements "abcabc" 3 >>= putStrLn -- '>>=' passes the result of the expression on the left as an argument to the expression on the right