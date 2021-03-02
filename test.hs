{-# OPTIONS_GHC -Wno-missing-methods #-}
module Lorenzo(
    module Lorenzo
) where
        import Control.Applicative
m        import Data.Char(isSpace, digitToInt)
        a :: Float
        a = 5 
        
        newtype Parser a = P( String -> [(a,String)])

        instance Functor Parser where
                -- fmap :: (a -> b) -> Parser a -> Parser b
                fmap g p = P(\inp -> case parse p inp of
                                [] -> []
                                [(v, out)] -> [(g v, out)]
                         )
                

        instance Applicative Parser where
        -- pure :: a -> Parser a
                pure v = P (\inp -> [(v,inp)])

                -- <*> :: Parser (a -> b) -> Parser a -> Parser b
                pg <*> px = P (\inp -> case parse pg inp of
                                        []        -> []
                                        [(g,out)] -> parse (fmap g px) out)

        instance Monad Parser where
                --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
                p >>= f = P (\inp -> case parse p inp of
                                        []        -> []
                                        [(v,out)] -> parse (f v) out)

        instance Alternative Parser where
                --empty :: Parser a
                empty = P (\inp -> [])

                --(<|>) :: Parser a -> Parser a -> Parser a
                p <|> q = P (\inp -> case parse p inp of
                                []        -> parse q inp
                                [(v,out)] -> [(v,out)])

        bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
        bind f' (gx,gs) = let (fx,fs) = f' gx in (fx,gs++fs)
        
        --------PARSER -----------
        
        item :: Parser Char
        item  = P(\inp -> case inp of
                        []     -> []
                        (x:xs) -> [(x,xs)])

        failure :: Parser a
        failure  = P(\inp -> [])

        -- return  :: a -> Parser a
        -- return v = P(\inp -> [(v,inp)])

        --     (+++)  :: Parser a -> Parser a -> Parser a
        --     p1 +++ q1 = P(\inp -> case p1 inp) of
        --                    []        -> parse q1 inp
        --                    [(v,out)] -> [(v,out)])

        parse :: Parser a -> String -> [(a,String)]
        parse (P p) inp = p inp             

        -- sequencing
        --     (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        --     p >>= f = \inp -> case (parse p inp) of
        --         [] -> []
        --         [(v,out)] -> parse (f v) out
        -- p :: Parser (Char,Char)
        -- p  = do x <- item
        --         ;y <- item
        --         ;Prelude.return (x,y)
        sat  :: (Char -> Bool) -> Parser Char
        sat p = item >>= \x -> if p x then return x else failure

        isDigit :: Char -> Bool
        isDigit c = c >= '0' && c <= '9'
        
        digit :: Parser Char
        digit  = sat isDigit

        char  :: Char -> Parser Char
        char x = sat (x ==)
        --uasge parse (char 'x') "cdxiao"
        nat :: Parser Int
        nat = some digit >>= \xs -> return (read xs)
        
        --     many2  :: Parser a -> Parser [a]
        --     many2 p = many1 p Lorenzo.+++ return []

        --     many1  :: Parser a -> Parser [a]
        --     many1 p = p >>= \x -> many2 p >>= \vs -> return (x:vs)

        string :: String -> Parser String
        string []     = return []
        string (x:xs) = char x >>= \y -> string xs >>= \ys -> return (y:ys)
        --usage parse (string "xcd") "xcdxiao"  

        xxx :: Parser Char
        xxx = char '.' >>= \c -> digit >>= \d -> return d
        
        --     p :: Parser String
        --     p  = char '[' >>= \x -> digit >>= \d -> many xxx >>= \ds -> char ']' >>= \y -> return (d:ds)
        
        space :: Parser ()
        space = many (sat isSpace) >>= \x -> return ()

        token :: Parser a -> Parser a
        token p = space >>= \x -> p >>= \v -> space >>= \s -> return v
                --  v <- p
                --  space
                --  return v

        symbol :: String -> Parser String
        symbol xs = token (string xs)

        factor :: Parser Int
        factor = 
                do 
                        d <- digit
                        return (digitToInt d)
                        <|> 
                        do 
                                char '('
                                e <- expr
                                char ')'
                                return e



        expr :: Parser Int
        expr = do
                t <- term        
                do              
                symbol "+"
                ;e <- expr
                ;return (t + e)
                <|> do              
                        symbol "-"
                        ;e <- expr
                        ;return (t - e)
                        <|> return t

        term :: Parser Int
        --term  = factor >>= \f -> (char '*' >>= \x -> term >>= \t -> return (f*t)) <|> return f
        term = do 
                factor >>= \f -> 
                        do 
                        char '*' >>= \c -> term >>= \t -> return (f * t)
                        <|> do 
                                char '/' >>= \c -> term >>= \t -> return (f `div`  t)
                                <|> return f

        eval   :: String -> Int
        eval xs = fst (head (parse expr xs))

        test :: Parser Int 
        test = char '1' >>= \c -> return (read [c])
        
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