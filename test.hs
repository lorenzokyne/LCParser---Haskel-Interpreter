module Lorenzo(
    
) where
    import Control.Applicative
    import Data.Char(isSpace)
    a :: Float
    a = 5

    x :: String
    x = "Ciao"
    bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
    bind f' (gx,gs) = let (fx,fs) = f' gx in (fx,gs++fs)
    
    --------PARSER -----------
    type Parser a = String -> [(a,String)]
    item :: Parser Char
    item  = \inp -> case inp of
                  []     -> []
                  (x:xs) -> [(x,xs)]

    failure :: Parser a
    failure  = \inp -> []

    return  :: a -> Parser a
    return v = \inp -> [(v,inp)]

    (+++)  :: Parser a -> Parser a -> Parser a
    p +++ q = \inp -> case p inp of
                   []        -> parse q inp
                   [(v,out)] -> [(v,out)]

    parse :: Parser a -> String -> [(a,String)]
    parse p inp = p inp               

    -- sequencing
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = \inp -> case (parse p inp) of
        [] -> []
        [(v,out)] -> parse (f v) out
    -- p :: Parser (Char,Char)
    -- p  = do x <- item
    --         ;y <- item
    --         ;Prelude.return (x,y)
    sat  :: (Char -> Bool) -> Parser Char
    sat p = item Lorenzo.>>= \x -> if p x then Lorenzo.return x else failure

    isDigit :: Char -> Bool
    isDigit c = c >= '0' && c <= '9'
    
    digit :: Parser Char
    digit  = sat isDigit

    char  :: Char -> Parser Char
    char x = sat (x ==)
    --uasge parse (char 'x') "cdxiao"
    
    nat :: Parser Int
    nat = do{
            xs <- some digit 
            ;Lorenzo.return (read xs)
            }  
    
    many2  :: Parser a -> Parser [a]
    many2 p = many1 p Lorenzo.+++ Lorenzo.return []

    many1  :: Parser a -> Parser [a]
    many1 p = p Lorenzo.>>= \x -> many2 p Lorenzo.>>= \vs -> Lorenzo.return (x:vs)

    string :: String -> Parser String
    string []     = Lorenzo.return []
    string (x:xs) = char x Lorenzo.>>= \y -> string xs Lorenzo.>>= \ys -> Lorenzo.return (y:ys)
    --usage parse (string "xcd") "xcdxiao"  

    xxx :: Parser Char
    xxx = char '.' Lorenzo.>>= \c -> digit Lorenzo.>>= \d -> Lorenzo.return d
    
    p :: Parser String
    p  = char '[' Lorenzo.>>= \x -> digit Lorenzo.>>= \d -> many2 xxx Lorenzo.>>= \ds -> char ']' Lorenzo.>>= \y -> Lorenzo.return (d:ds)
    
    space :: Parser ()
    space = many2 (sat isSpace) Lorenzo.>>= \x -> Lorenzo.return ()

    token :: Parser a -> Parser a
    token p = space Lorenzo.>>= \x -> p Lorenzo.>>= \v -> space Lorenzo.>>= \s -> Lorenzo.return v
            --  v <- p
            --  space
            --  return v

    symbol :: String -> Parser String
    symbol xs = token (string xs)

    factor :: Parser Int
    factor = symbol "(" Lorenzo.>>= \x -> expr Lorenzo.>>= \e -> symbol ")" Lorenzo.>>= \y -> Lorenzo.return e 
            --     e <- expr
            --     symbol ")"
            --     return e
            -- <|> natural

    expr :: Parser Int
    expr = term Lorenzo.>>= \t -> (char '+' Lorenzo.>>= \x -> expr Lorenzo.>>= \e -> Lorenzo.return (t + e)) +++ Lorenzo.return 5

    term :: Parser Int
    term  = factor Lorenzo.>>= \f -> (char '*' Lorenzo.>>= \x -> term Lorenzo.>>= \t -> Lorenzo.return (f*t)) +++ Lorenzo.return f
            -- do char '*'
            --     t  term
            --     return (f * t)
            --     +++ return f 

    eval   :: String -> Int
    eval xs = fst (head (parse expr xs))

    test :: Parser Int 
    test = char '1' Lorenzo.>>= \c -> Lorenzo.return (read [c])
