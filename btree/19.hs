import BTree
import Prelude hiding ((<=))

rewrite :: BTree a -> BTree a -> BTree a 
rewrite Empty Empty = Empty
rewrite Empty _ = Empty
rewrite (Branch x left right) Empty = Branch x left right  
rewrite (Branch x left1 right1) (Branch y left2 right2) = Branch y (rewrite left1 left2 ) (rewrite right1 right2)

infixl 4 <= 

(<=) :: BTree a -> BTree a -> BTree a
(<=) = rewrite

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
    print(a<=b)