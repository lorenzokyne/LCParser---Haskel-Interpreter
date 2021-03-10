{-# OPTIONS_GHC -Wno-missing-methods #-}
module Lorenzo(
    module Lorenzo
) where
        import Control.Applicative
        import Data.Char(isSpace, digitToInt, isAlphaNum, isLetter, intToDigit)
        a :: Float
        a = 5

        newtype Parser a = P(Environment->  String -> [(Environment,a,String)])
        
        --environment is composed of three string:
        --Variable name
        --Variable type
        --Variable value

        boolType = "Boolean"
        intType = "Integer"
        type Environment = [(Char, String, String)]

        getName :: (Char, String, String) -> Char
        getName (name,_,_) = name

        getType :: (Char, String, String) -> String
        getType (_,vartype,_) = vartype

        getValue :: (Char, String, String) -> String
        getValue (_,_,value) = value

        updateEnv :: Char -> String -> String -> Parser String
        updateEnv varName varType varValue = 
                 P(\env inp -> case inp of
                        xs -> [(modifyEnv env varName varType varValue,"",xs)])

        modifyEnv :: Environment -> Char -> String -> String -> Environment
        modifyEnv [] varName varType varValue = [(varName,varType,varValue)]
        modifyEnv xs varName varType varValue = if getName (head xs) == varName
                                                        then [(varName,varType,varValue)] ++ tail xs
                                                    else [head xs] ++ modifyEnv (tail xs) varName varType varValue
        
        --get the value of a named var in the environment
        getVariableValue :: Char -> Parser String
        getVariableValue varname = P(\env inp -> [(env,snd (getVariable env varname),inp)])

        --get the value of a named var in the environment
        getVariableType :: Char -> Parser String
        getVariableType varname = P(\env inp -> [(env,fst (getVariable env varname),inp)])

        getVariable :: Environment -> Char -> (String,String)
        getVariable [] _ = ("","")
        getVariable env varname =  if getName (head env) == varname then (getType (head env),getValue (head env)) else getVariable (tail env) varname  

        instance Functor Parser where
                -- fmap :: (a -> b) -> Parser a -> Parser b
                fmap g p = P(\env inp -> case parse p env inp of
                                [] -> []
                                [(env, v, out)] -> [(env, g v, out)]
                         )


        instance Applicative Parser where
        -- pure :: a -> Parser a
                pure v = P (\env inp -> [(env,v,inp)])
                -- <*> :: Parser (a -> b) -> Parser a -> Parser b
                pg <*> px = P (\env inp -> case parse pg env inp of
                                        []        -> []
                                        [(env, g,out)] -> parse (fmap g px) env out)

        instance Monad Parser where
                --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
                p >>= f = P (\env inp -> case parse p env inp of
                                        []        -> []
                                        [(env,v,out)] -> parse (f v) env out)

        instance Alternative Parser where
                --empty :: Parser a
                empty = P (\env inp -> [])
                --(<|>) :: Parser a -> Parser a -> Parser a
                p <|> q = P (\env inp -> case parse p env inp of
                                []        -> parse q env inp
                                [(env,v,out)] -> [(env,v,out)])

        bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
        bind f' (gx,gs) = let (fx,fs) = f' gx in (fx,gs++fs)

        --------PARSER -----------

        item :: Parser Char
        item  = P(\env inp -> case inp of
                        []     -> []
                        (x:xs) -> [(env,x,xs)])

        failure :: Parser a
        failure  = P(\env inp -> [])

        -- return  :: a -> Parser a
        -- return v = P(\inp -> [(v,inp)])

        --     (+++)  :: Parser a -> Parser a -> Parser a
        --     p1 +++ q1 = P(\inp -> case p1 inp) of
        --                    []        -> parse q1 inp
        --                    [(v,out)] -> [(v,out)])

        parse :: Parser a -> Environment -> String -> [(Environment,a,String)]
        parse (P p) env inp = p env inp

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

        identifier :: Parser Char
        identifier = sat isLetter  

        --uasge parse (char 'x') "cdxiao"
        nat :: Parser Int
        nat = some digit >>= \xs -> return (read xs)

        int :: Parser Int
        int = nat <|> do
                symbol "-"
                ;n <- nat
                ;return (-n)

        assignment :: Parser String
        assignment = do
                id <- identifier
                symbol "="
                e <- expr
                updateEnv id intType (show e)
                symbol ";"
                return (show e)
                <|>
                do
                        id <- identifier
                        symbol "="
                        b <- bexprAND
                        updateEnv id boolType (show b)
                        symbol ";"
                        return (show b)

        --     many2  :: Parser a -> Parser [a]
        --     many2 p = many1 p Lorenzo.+++ return []

        --     many1  :: Parser a -> Parser [a]
        --     many1 p = p >>= \x -> many2 p >>= \vs -> return (x:vs)

        string :: String -> Parser String
        string []     = return []
        string (x:xs) = char x >>= \y -> string xs >>= \ys -> return (y:ys)
        --usage parse (string "xcd") "xcdxiao"

        space :: Parser ()
        space = many (sat isSpace) >>= \x -> return ()

        token :: Parser a -> Parser a
        token p = do
                space
                v <- p
                space
                return v
        
        --Parse a line of comment string and return the remaining string 
        comment :: Parser ()
        comment = string "--" >>= \c -> many (sat isAlphaNum) >>= \i -> string "\n" >>= \end -> return ()

        symbol :: String -> Parser String
        symbol xs = token (string xs)

        compareTo :: Parser Bool 
        compareTo = do
                left <- expr
                symbol "<"
                right <- expr
                return (left < right)
                <|>
                do
                left <- expr
                symbol ">"
                right <- expr
                return (left > right)
                <|>
                do
                left <- expr
                symbol "=="
                right <- expr
                return (left == right)
                <|>
                do
                left <- expr
                symbol "<>"
                right <- expr
                return (left /= right)

        bexpr :: Parser Bool
        bexpr = do 
                symbol "True"
                return True 
                <|>
                do
                symbol "False"
                return False
                <|>
                do
                        symbol "("
                        b <-bexprAND
                        symbol ")"
                        return b
                        <|>
                        compareTo
                        <|>
                                do
                                space
                                id <- identifier
                                space
                                value <- getVariableValue id
                                if value == "" then failure else
                                        if (value == "True" || value /= "0") then return True else return False

        bexprAND :: Parser Bool
        bexprAND =do
                        b1 <- bexprOR
                        symbol "AND"
                        b2 <- bexprAND
                        return (b1 && b2)
                        <|>
                        bexprOR

        bexprOR :: Parser Bool
        bexprOR =do
                        b1 <- bexpr
                        symbol "OR"
                        b2 <- bexprOR
                        return (b1 || b2)
                        <|>
                        bexpr

        -- bexpr :: Parser Bool 
        -- bexpr = do
        --                 b1 <- booleans
        --                 symbol "AND"
        --                 b2 <- booleans
        --                 return (b1 && b2)
        --                 <|>
        --                 do
        --                 b1 <- booleans
        --                 symbol "OR"
        --                 b2 <- booleans
        --                 return (b1 || b2)
        --                 <|>
        --                 do booleans

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
        term = do
                factor >>= \f ->
                        do
                        symbol "*" >>= \c -> term >>= \t -> return (f * t)
                        <|> do
                                symbol "/" >>= \c -> term >>= \t -> return (f `div`  t)
                                <|> return f

        factor :: Parser Int
        factor =
                do
                        int
                        <|>
                        do
                                symbol "("
                                e <- expr
                                symbol ")"
                                return e
                                <|>
                                do
                                space
                                id <- identifier
                                space
                                vartype <- getVariableType id
                                if vartype == intType then 
                                        do
                                        value <- getVariableValue id
                                        return (read value) 
                                else failure
        
        cmd :: Parser String
        cmd = assignment
                <|>
                ifThenElse

        program :: Parser String
        program = do
                cmd;
                program
                <|>
                cmd

        ifThenElse :: Parser String
        ifThenElse = do
                        symbol "if"
                        condition <- bexprAND
                        symbol "then"
                        if condition then do 
                                a<-program
                                symbol "else"
                                b<-parseProgram
                                symbol "endif"
                                return a 
                        else 
                                do
                                a<-parseProgram
                                symbol "else"
                                b<-program
                                symbol "endif"
                                return b

        --temp = do symbol "if"; condition <- bexprAND; return condition

        --temp = do symbol "if"; condition <- bexprAND; symbol "then";return condition
        temp = do symbol "if"; condition <- bexprAND; symbol "then";a<-assignment;return condition
        parseTemp = do symbol "if";condition <- parseBexprAND; symbol "then";a<-assignment;return (condition++a);
        
        -- temp = do symbol "if"; condition <- bexprAND; return condition
        -- while :: Parser String
        -- while = do 
        --         symbol "while"
        --         condition <- bexprAND
        --         if condition then do
        --                 assignment
        --                 while
        --                 else return ""

        -- eval   :: String -> Int
        -- eval xs = fst (head (parse expr xs))

        --------------------
        --PARSE-------------
        --------------------
        parseNat :: Parser String
        parseNat = some digit >>= \xs -> return xs

        parseInt :: Parser String
        parseInt = parseNat <|> do
                symbol "-"
                ;n <- parseNat
                ;return ("-"++n)

        parseFactor :: Parser String
        parseFactor = 
                do
                                d <- parseInt
                                return d
                                <|>
                                do
                                        symbol "("
                                        e <- parseExpr
                                        symbol ")"
                                        return e
                                        <|>
                                        do
                                        space
                                        id <- identifier
                                        space
                                        vartype <- getVariableType id
                                        if vartype == intType then return [id]
                                        else failure
        parseTerm :: Parser String
        parseTerm = do
                        parseFactor >>= \f ->
                                do
                                symbol "*" >>= \c -> parseTerm >>= \t -> return (f ++ "*" ++ t)
                                <|> do
                                        symbol "/" >>= \c -> parseTerm >>= \t -> return (f ++ "/" ++  t)
                                        <|> return f

        parseExpr :: Parser String
        parseExpr =do
                t <- parseTerm
                do
                        symbol "+"
                        ;e <- parseExpr
                        ;return (t ++ "+" ++ e)
                        <|> do
                                symbol "-"
                                ;e <- parseExpr
                                ;return (t ++ "-" ++ e)
                                <|> return t
                                
        parseBexprAND :: Parser String
        parseBexprAND = do
                        b1 <- parseBexprOR
                        symbol "OR"
                        b2 <- parseBexprAND
                        return (b1 ++ "OR" ++ b2)
                        <|>
                        parseBexprOR

        parseBexprOR :: Parser String
        parseBexprOR = do
                        b1 <- parseBexpr
                        symbol "AND"
                        b2 <- parseBexprOR
                        return (b1 ++ "AND" ++ b2)
                        <|>
                        parseBexpr
        
        parseBexpr :: Parser String
        parseBexpr = do 
                symbol "True"
                return "True"
                <|>
                do
                symbol "False"
                return "False"
                <|>
                do
                        symbol "("
                        b <-parseBexprAND
                        symbol ")"
                        return ("(" ++ b ++ ")")
                        <|>
                        parseCompareTo
                        <|>
                                do
                                id <- identifier
                                return [id]

        parseCompareTo :: Parser String 
        parseCompareTo = do
                left <- parseExpr
                symbol "<"
                right <- parseExpr
                return (left ++ "<" ++ right)
                <|>
                do
                left <- parseExpr
                symbol ">"
                right <- parseExpr
                return (left ++ ">" ++ right)
                <|>
                do
                left <- parseExpr
                symbol "=="
                right <- parseExpr
                return (left ++ "==" ++ right)
                <|>
                do
                left <- parseExpr
                symbol "<>"
                right <- parseExpr
                return (left ++ "/=" ++ right)

        parseAssignment :: Parser String
        parseAssignment = do
                id <- identifier
                symbol "="
                e <- parseExpr
                symbol ";"
                return ([id] ++ "=" ++ e ++ ";")
                <|>
                do
                        id <- identifier
                        symbol "="
                        b <- parseBexprAND
                        updateEnv id boolType (show b)
                        return ([id] ++ "=" ++ b ++ ";")
        
        parseIfThenElse :: Parser String            
        parseIfThenElse = do
                symbol "if"
                condition <- parseBexprAND
                symbol "then"
                a <- parseProgram
                symbol "else"
                b <- parseProgram
                symbol "endif"
                return ("if " ++ condition ++ " then " ++ a ++ " else " ++ b ++ "endif")

        parseCmd :: Parser String
        parseCmd = do
                parseAssignment
                <|>
                parseIfThenElse
        
        parseProgram :: Parser String
        parseProgram = do
                parseCmd
                parseProgram
                <|>
                parseCmd
        
        
----------------------------------------------------------

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
