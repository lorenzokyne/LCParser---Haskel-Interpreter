{-# OPTIONS_GHC -Wno-missing-methods #-}
module Lorenzo(
    module Lorenzo
) where
        import Control.Applicative
        import Data.Char(isSpace)
        a :: Float
        a = 5 
        
        newtype Parser a = P( String -> [(a,String)])

        {-# LANGUAGE FlexibleInstances #-}
        instance Functor Parser where
        fmap :: (a -> b) -> Parser a -> Parser b
        fmap g p = P(\inp -> case parse p inp of
                                []        -> []
                                [(v,out)] -> [(g v, out)])  
        instance Applicative Parser where
        -- pure :: a -> Parser a
        pure v = P (\inp -> [(v,inp)])

        -- <*> :: Parser (a -> b) -> Parser a -> Parser b
        pg <*> px = P (\inp -> case parse pg inp of
                                []        -> []
                                [(g,out)] -> parse (Lorenzo.fmap g px) out)
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
        factor = symbol "(" >>= \x -> (expr >>= \e -> symbol ")" >>= \y -> return e) <|> nat
                --     e <- expr
                --     symbol ")"
                --     return e
                -- <|> natural

        expr :: Parser Int
        expr = term >>= \t -> 
                        do {
                                symbol "+"
                                ;e <- expr
                                ;return (t + e)
                        }
                <|> return t

        term :: Parser Int
        --term  = factor >>= \f -> (char '*' >>= \x -> term >>= \t -> return (f*t)) <|> return f
        term = factor >>= \f -> (char '*' >>= \c -> term >>= \t -> return (f * t)) <|> return f 

        eval   :: String -> Int
        eval xs = fst (head (parse expr xs))

        test :: Parser Int 
        test = char '1' >>= \c -> return (read [c])
