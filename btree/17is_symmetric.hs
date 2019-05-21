import BTree

is_balanced Empty Empty = True
is_balanced (Branch _ left1 right1) (Branch _ left2 right2) = is_balanced left1 right2 && is_balanced right1 left2
is_balanced _ _ = False

is_symmetric :: BTree a -> Bool
is_symmetric Empty = True
is_symmetric (Branch _ left right) = is_balanced left right

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
    print(is_symmetric tree)
    let tree2 = Branch 7 (Branch 6 (Branch 5 Empty Empty) (Branch 8 Empty Empty)) (Branch 2 (Branch 3 Empty Empty) Empty)
{-
           7
          / \
        /     \
      /         \
     6           2
   /   \       /   \
  5     8     3     *
 / \   / \   / \ 
*   * *   * *   *
-}
    print(is_symmetric tree2)