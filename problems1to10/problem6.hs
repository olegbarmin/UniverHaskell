-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

main = do
    print (isPalindrome "abc")
    print (isPalindrome "abccba")
    print (isPalindrome [4,2,3])
    print (isPalindrome [3,2,3])