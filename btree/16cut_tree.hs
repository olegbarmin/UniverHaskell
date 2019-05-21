import BTree

chop_tree :: BTree a ->Int -> BTree a
chop_tree _ 0 = Empty
chop_tree Empty _ = Empty
chop_tree (Branch x left right) n = Branch x (chop_tree left (n-1)) (chop_tree right (n-1))


main = do
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
    let tree = Branch 7 (Branch 6 (Branch 5 Empty Empty) (Branch 8 Empty Empty)) (Branch 2 (Branch 3 Empty Empty) (Branch 4 Empty Empty))
    print(chop_tree tree 0)
    print(chop_tree tree 1)
    print(chop_tree tree 2)