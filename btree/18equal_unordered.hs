import BTree

equals :: Eq a => BTree a -> BTree a -> Bool
equals (Branch x a b) (Branch y c d) = x == y && ((equals a c && equals b d) || (equals a d && equals b c))
equals Empty Empty = True
equals _ _ = False

main = do
{-
           1
          / \
        /     \
      /         \
     2           4
   /   \       /   \
  3     *     *     *
 / \  
*   *
-}
    let a = Branch 1 (Branch 2 (Branch 3 Empty Empty) Empty) (Branch 4 Empty Empty)
{-
           1
          / \
        /     \
      /         \
     4           2
   /   \       /   \
  *     *     3     *
             / \
            *   *
-}
    let b = Branch 1 (Branch 4 Empty Empty) (Branch 2 Empty (Branch 3 Empty Empty))
{-
           1
          / \
        /     \
      /         \
     2           *
   /   \       
  3     4  
 / \   / \
*   * *   *
-}
    let c = Branch 1 (Branch 2 (Branch 3 Empty Empty) (Branch 4 Empty Empty)) Empty
    print(equals a b)
    print(equals a c)