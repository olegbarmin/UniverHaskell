-- Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a] -- новый тип, елементы которого могут быть либо елементами либо списками из структур

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ flatten(List xs)


main = do
    print (flatten $ List [ Elem 1, Elem 2, Elem 3])
    print (flatten $ List [ Elem 1, List [Elem 4, Elem 5], Elem 3])