module BTree where

data BTree a = Empty | Branch a (BTree a) (BTree a)
    deriving (Show,Eq)