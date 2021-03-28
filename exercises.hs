-----------------------------------------------------------

        moltiplica :: Int -> Int-> Int
        moltiplica _ 0 = 0
        moltiplica x n = x + moltiplica x (n-1)

        --chapter 10 exercises

        safediv :: Int -> Int -> Maybe Int
        safediv _ 0 = Nothing
        safediv x n = Just (x `div` n)

        data Nat = Zero | Succ Nat

        nat2int         :: Nat -> Int
        nat2int Zero     = 0
        nat2int (Succ n) = 1 + nat2int n

        data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

        data Tree = Leaf (Maybe Int)
          | Node Tree (Maybe Int) Tree

        occurs               :: Maybe Int -> Tree -> Bool
        occurs m (Leaf n)     = m==n
        occurs m (Node l n r) = m==n
                                || occurs m l
                                || occurs m r

        flatten :: Tree -> [(Maybe Int)]
        flatten (Leaf l) = [l]
        flatten (Node l i r) = flatten l ++ [i] ++ flatten r

        occursFaster m (Leaf n)            = m==n
        occursFaster m (Node l n r) | m==n = True
                        | m<n  = occursFaster m l
                        | m>n  = occursFaster m r

        albero = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (Just 3)) (Just 4) (Leaf (Just 5))) (Just 6) (Leaf (Just 7))) (Just 8) (Leaf (Just 9))) (Just 10) (Leaf (Just 11))) (Just 12) (Leaf (Just 13)) ) (Just 14) (Leaf (Just 15)) ) (Just 16) (Leaf (Just 17)) ) (Just 18) (Leaf (Just 19))) (Just 20) (Leaf (Just 21))) (Just 22) (Leaf (Just 23))) (Just 24) (Leaf (Just 25))  ) (Just 26)(Leaf (Just 27))  ) (Just 28) (Leaf (Just 29))
        albero1 = Node (Node (Leaf (Just 3)) (Just 4) (Leaf (Just 5 ))) (Just 4) (Leaf (Just 5 ))
        albero2 = Node (Node (Leaf (Just 1)) (Just 3) (Leaf (Just 4))) (Just 5) (Node (Leaf (Just 6)) (Just 7) (Leaf (Just 9)))

        contaFigliSinistra :: Tree -> Int
        contaFigliSinistra (Leaf l) = 0
        contaFigliSinistra (Node l c r) = length (flatten l)

        contaFigliDestra :: Tree -> Int
        contaFigliDestra (Leaf l) = 0
        contaFigliDestra (Node l c r) = length (flatten r)

        isComplete :: Tree -> Bool
        isComplete (Leaf l) = True
        isComplete (Node l c r) = length (flatten l) == length (flatten r)
