import BTree 

leftmost :: BTree a -> a
leftmost (Branch x Empty _) = x
leftmost (Branch _ left _) = leftmost left


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
    print(leftmost tree)