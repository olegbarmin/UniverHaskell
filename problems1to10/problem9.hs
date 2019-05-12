pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (a, b) = span (== x) xs in (x : a) : pack b -- span разделет список на кортеж из елементов пройденных в списке, до тех пор пока условие сохранялось

main = do
    print(pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e'])