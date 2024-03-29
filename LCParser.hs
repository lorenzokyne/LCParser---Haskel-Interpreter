module LC() where
        import Control.Applicative
        import Data.Char(isSpace, digitToInt, isAlphaNum, isLetter, intToDigit, isPrint, isAscii, isSymbol)

        newtype Parser a = P(Environment->  String -> [(Environment,a,String)])
        ---CONSTANTS
        boolType = "Boolean"
        intType = "Integer"
        ------------        
        --environment is composed of three string:
        --Variable name
        --Variable type
        --Variable value
        type Environment = [(Char, String, String)]

        getName :: (Char, String, String) -> Char
        getName (name,_,_) = name

        getType :: (Char, String, String) -> String
        getType (_,vartype,_) = vartype

        getValue :: (Char, String, String) -> String
        getValue (_,_,value) = value

        changeEnvironment :: Char -> String -> String -> Parser String
        changeEnvironment varName varType varValue = 
                 P(\env inp -> case inp of
                        xs -> [(modifyEnvironment env varName varType varValue,"",xs)])

        modifyEnvironment :: Environment -> Char -> String -> String -> Environment
        modifyEnvironment [] varName varType varValue = [(varName,varType,varValue)]
        modifyEnvironment xs varName varType varValue = if getName (head xs) == varName
                                                        then [(varName,varType,varValue)] ++ tail xs
                                                    else [head xs] ++ modifyEnvironment (tail xs) varName varType varValue
        
        --get the value of a named var in the environment
        getVariableValue :: Char -> Parser String
        getVariableValue varname = P(\env inp -> [(env,snd (getVariable env varname),inp)])

        --get the type of a named var in the environment
        getVariableType :: Char -> Parser String
        getVariableType varname = P(\env inp -> [(env,fst (getVariable env varname),inp)])
        
        --get the value and type of a named var in the environment
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

        parse :: Parser a -> Environment -> String -> [(Environment,a,String)]
        parse (P p) env inp = p env inp

        sat  :: (Char -> Bool) -> Parser Char
        sat p = item >>= \x -> if p x then return x else failure
        
        isDigit :: Char -> Bool
        isDigit c = c >= '0' && c <= '9'

        isComment :: Char -> Bool
        isComment c = isPrint c && not (c=='/')

        digit :: Parser Char
        digit  = sat isDigit

        char  :: Char -> Parser Char
        char x = sat (x ==)

        identifier :: Parser Char
        identifier = sat isLetter  

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
                symbol ";"
                changeEnvironment id intType (show e)
                return (show e)
                <|>
                do
                        id <- identifier
                        symbol "="
                        b <- bexprAND
                        symbol ";"
                        changeEnvironment id boolType (show b)
                        return (show b)

        --     many2  :: Parser a -> Parser [a]
        --     many2 p = many1 p LC.+++ return []

        --     many1  :: Parser a -> Parser [a]
        --     many1 p = p >>= \x -> many2 p >>= \vs -> return (x:vs)

        string :: String -> Parser String
        string []     = return []
        string (x:xs) = char x >>= \y -> string xs >>= \ys -> return (y:ys)
        --usage: parse (string "name") "name+ecc"

        space :: Parser ()
        space = many (sat isSpace) >>= \x -> return ()

        token :: Parser a -> Parser a
        token p = do
                space
                v <- p
                space
                return v
        
        --Parse a line of comment string and return the remaining string 
        comment :: Parser String
        comment = string "--" >>= \c -> many (sat isComment) >>= \i -> string "/--" >>= \end -> return ""

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
                        b <-bexprOR
                        symbol ")"
                        return b
                        <|>
                        compareTo
                        <|>
                        do
                        symbol "NOT"
                        b<-bexprOR
                        return (not b)
                        <|>
                                do
                                space
                                id <- identifier
                                space
                                value <- getVariableValue id
                                if value == "" then failure else
                                        if (value == "True" || (value/="False" && value /= "0")) then return True else return False

        bexprAND :: Parser Bool
        bexprAND =do
                        b1 <- bexpr
                        symbol "AND"
                        b2 <- bexprAND
                        return (b1 && b2)
                        <|>
                        bexpr

        bexprOR :: Parser Bool
        bexprOR =do
                        b1 <- bexprAND
                        symbol "OR"
                        b2 <- bexprOR
                        return (b1 || b2)
                        <|>
                        bexprAND

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
        --equivalent to above but with different syntax
        -- term :: Parser Int
        -- term = do
        --         f <- factor
        --         do
        --         symbol "*" 
        --         t <- term 
        --         return (f * t)
        --         <|> do
        --                 symbol "/" 
        --                 t <- term 
        --                 return (f `div` t)
        --                 <|> 
        --                 return f                        

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
                <|>
                while
                <|>
                comment

        program :: Parser String
        program = do
                cmd;
                program
                <|>
                cmd

        ifThenElse :: Parser String
        ifThenElse = do
                        symbol "if"
                        condition <- bexprOR
                        symbol "then"
                        if condition then do 
                                a<-program
                                symbol "else"
                                b<-parseProgram
                                symbol "endif"
                                return a 
                                <|>
                                do 
                                a<-program
                                symbol "endif"
                                return a 
                        else 
                                do
                                a<-parseProgram
                                symbol "else"
                                b<-program
                                symbol "endif"
                                return b
                                <|>
                                do
                                a<-parseProgram
                                symbol "endif"
                                return "" 
                                
        duplicateWhile :: String -> Parser String 
        duplicateWhile c = P(\env inp -> [(env,"",c ++ " " ++ inp)])

        while :: Parser String
        while = do 
                whileString <- parseWhile
                duplicateWhile whileString
                symbol "while"
                condition <- bexprOR
                symbol "do"
                if condition then do
                        program
                        symbol "endWhile"
                        duplicateWhile whileString
                        while
                else do
                        parseProgram
                        symbol "endWhile"
                        return ""

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
                                        return [id]
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
                        b1 <- parseBexpr
                        symbol "AND"
                        b2 <- parseBexprAND
                        return (b1 ++ "AND" ++ b2)
                        <|>
                        parseBexpr
        
        parseBexprOR :: Parser String
        parseBexprOR = do
                        b1 <- parseBexprAND
                        symbol "OR"
                        b2 <- parseBexprOR
                        return (b1 ++ "OR" ++ b2)
                        <|>
                        parseBexprAND


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
                        b <-parseBexprOR
                        symbol ")"
                        return ("(" ++ b ++ ")")
                        <|>
                        parseCompareTo
                        <|>
                        do
                                symbol "NOT"
                                b<-parseBexprOR
                                return ("NOT " ++ b)
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
                        symbol ";"
                        return ([id] ++ "=" ++ b ++ ";")
        
        parseIfThenElse :: Parser String            
        parseIfThenElse = do
                symbol "if"
                condition <- parseBexprOR
                symbol "then"
                a <- parseProgram
                symbol "else"
                b <- parseProgram
                symbol "endif"
                return ("if " ++ condition ++ " then " ++ a ++ " else " ++ b ++ " endif")
                <|>
                do
                symbol "if"
                condition <- parseBexprOR
                symbol "then"
                a <- parseProgram
                symbol "endif"
                return ("if " ++ condition ++ " then " ++ a ++ " endif")

        parseCmd :: Parser String
        parseCmd = do
                parseAssignment
                <|>
                parseIfThenElse
                <|>
                parseWhile
                <|>
                parseComment

        parseProgram :: Parser String
        parseProgram = do
                cmd<-parseCmd
                progr<-parseProgram
                return (cmd ++ progr)
                <|>
                parseCmd

        parseWhile = do
                symbol "while"
                c <- parseBexprOR 
                symbol "do"
                p <- parseProgram
                symbol "endWhile"
                return ("while " ++ c ++ " do " ++ p ++ " endWhile ")

        parseComment = do
                string "--" 
                i <- many (sat isComment) 
                string "/--"
                return ("--" ++ i ++ "/--")

        --parseComment = string "--" >>= \c -> many (sat isComment) >>= \i -> string "/--" >>= \end -> return (c ++ i ++ end)

-----------------------------------------------------------
------------------------Interpreter------------------------

        programMemory :: [(Environment,String,String)] -> String
        programMemory [] = ""
        programMemory [([],_,_)] = ""
        programMemory [(env,inputString,"")] = "Variable name: " ++ [getName (head env)] ++ " - Variable value: " ++ getValue (head env) ++ "\n" ++ programMemory [((tail env),inputString,"")]
        programMemory [(env,inputString,outputString)] =  "\x1b[31m" ++ "Error occurred -> "++outputString ++"\x1b[0m\n"
        
        executeProgram :: [(Environment,String,String)] -> String
        executeProgram [] = "Invalid program\n"
        executeProgram [(env,inputString,"")] = "\nParsed program:\n" ++ inputString ++"\n\nMemory:\n" ++ programMemory (parse program [] inputString)
        executeProgram [(env,inputString,x)] = "\x1b[31m" ++ "Error: " ++ x ++"\x1b[0m\n"

        showHeader = do
                putStrLn ("\x1b[32m" ++"██╗      ██████╗██████╗  █████╗ ██████╗ ███████╗███████╗██████╗")
                putStrLn ("\x1b[32m" ++"██║     ██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗")
                putStrLn ("\x1b[0m"++  "██║     ██║     ██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝")
                putStrLn ("\x1b[0m"++  "██║     ██║     ██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗")
                putStrLn ("\x1b[31m" ++"███████╗╚██████╗██║     ██║  ██║██║  ██║███████║███████╗██║  ██║")
                putStrLn ("\x1b[31m" ++"╚══════╝ ╚═════╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝"++"\x1b[0m")

        executeInterpreter =do
                putStrLn "Enter the program to parse or type 'quit'"
                input <- getLine
                if input == "quit" then return "Thanks for using LCParser!" else do
                        putStrLn (executeProgram (parse parseProgram [] input))
                        executeInterpreter                                                     

        main = do
                showHeader
                executeInterpreter