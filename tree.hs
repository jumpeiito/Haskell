data Tree a = Nil | Node a (Tree a) (Tree a) 

instance Show (Tree a) where
  show Nil = " "
  show (Node x l r) =
    "Node " ++ (show x) ++ "-> " ++ (show l) ++ "--> " ++ (show r)

search :: Ord a => a -> Tree a -> Maybe (Tree a)
search _ Nil = Nothing
search n (Node a left right)
  | n == a = Just (Node a left right)
  | n > a  = search n left
  | n <= a = search n right

testcase = Node 12 (Node 15 (Node 20 Nil Nil) Nil) (Node 9 (Node 11 Nil Nil) Nil)
