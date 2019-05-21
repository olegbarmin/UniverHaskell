import BTree
import Prelude hiding ((^))

change :: BTree a -> BTree a -> BTree a 
change Empty Empty = Empty
change Empty tree = tree
change  (Branch x left right) tree = Branch x (change left tree) (change right tree) 

infixl 6 ^

(^) :: BTree a -> BTree a -> BTree a
(^) = change

main = do
{-
           1
          / \
         *   *
-}
    let a = Branch 1 Empty Empty
{-
           6
          / \
         *   *
-}
    let b = Branch 6 Empty Empty
    print(a^b)