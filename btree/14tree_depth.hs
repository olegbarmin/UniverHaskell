import BTree 

treewalk_depth :: BTree a -> [a]
treewalk_depth Empty = []
treewalk_depth (Branch x left right) = [x] ++ treewalk_depth(left) ++ treewalk_depth(right)


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
    print(treewalk_depth tree)