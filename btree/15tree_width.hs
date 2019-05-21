import BTree

treewalk_width :: BTree a -> [a]
treewalk_width Empty = []
treewalk_width tree = helper [tree]
    where helper [] = []
          helper (Empty:xs) = helper xs
          helper (Branch x left right : xs) = x : helper (xs ++ [left, right])


main = do
    let tree = Branch 7 (Branch 6 (Branch 5 Empty Empty) (Branch 8 Empty Empty)) (Branch 2 (Branch 3 Empty Empty) (Branch 4 Empty Empty))
{-
           7
          / \
        /     \
      /         \
     6           2
   /   \       /   \
  5     8     3     4
 / \   / \   / \   / \
*   * *   * *   * *   *
-}
    print(treewalk_width tree)
