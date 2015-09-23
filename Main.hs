module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Numeric
import Data.Ratio
import Data.Complex

data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool
        | Character Char
        | Float Float
        | Ratio Rational -- TODO: Why not Rational Ratio here? Ratio is from data.ratio,
        -- shouldn't rational be first since it's our data type?
        -- ANS: Rational is Ratio Integer, where Ratio is a Integral => a :% a (why :%?)
         | Complex (Complex Double)

instance Show LispVal where show = showVal

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
--main = do
--    (expr:_) <- getArgs
--    putStrLn (readExpr expr)
    --name <- getLine
    --putStrLn(name)
    --args <- getArgs
    --putStrLn("Hello, " ++ args !! 0 ++ " and " ++ args !! 1)
    --putStrLn("Result is " ++ show(read(args !! 0) + read(args !! 1)))

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
--TODO: Eval other types

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", unaryOp boolcheck),
              ("string?", unaryOp stringcheck),
              ("number?", unaryOp numbercheck),
              ("list?", unaryOp listcheck),
              ("char?", unaryOp charcheck),
              -- TODO: Handle number supersets correctly
              ("real?", unaryOp floatcheck),
              ("rational?", unaryOp ratiocheck),
              ("complex?", unaryOp complexcheck),
              ("symbol?", unaryOp symcheck),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

--unaryOp: Takes a function to check if a lispval is of a given
-- type that returns a bool lispval. Then applies that function
-- to the first element in the list of lispvals and returns the
-- result
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp constructorcheck [value] = constructorcheck value

-- boolcheck: checks if a lispval is a bool
-- stringcheck: checks if a lispval is a string
-- numbercheck: checks if a lispval is a number
-- listcheck: checks if a lispval is a list
-- charcheck: checks if a lispval is a char
-- floatcheck: checks if a lispval is a float
-- ratiocheck: checks if a lispval is a ratio
-- complexcheck: checks if a lispval is a complex
-- symcheck: checks if a lispval is a symbol/atom
boolcheck, stringcheck, numbercheck, listcheck, charcheck, floatcheck, ratiocheck, complexcheck, symcheck :: LispVal -> LispVal
boolcheck (Bool _) = Bool True
boolcheck _ = Bool False
stringcheck (String _) = Bool True
stringcheck _ = Bool False
numbercheck (Number _) = Bool True
numbercheck _ = Bool False
listcheck (List _) = Bool True
listcheck (DottedList _ _) = Bool True
listcheck _ = Bool False
charcheck (Character _) = Bool True
charcheck _ = Bool False
floatcheck (Float _) = Bool True
floatcheck _ = Bool False
ratiocheck (Ratio _) = Bool True
ratiocheck _ = Bool False
complexcheck (Complex _) = Bool True
complexcheck _ = Bool False
symcheck (Atom _) = Bool True
symcheck _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--F@ck weak typing!
--unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
--                           if null parsed
--                                then 0
--                                else fst $ parsed !! 0
--unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

--TODO: How does this typing work?
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--TODO: How does this typing work?
spaces :: Parser ()
spaces = skipMany1 space

--vowel :: Parser Char
--vowel = oneOf "aeiou"

readExpr :: String -> LispVal
--TODO: How does parse work? look at types ans:
-- parse takes an expresion to match against, a (seemingly arbitrary) name to represent this
-- value, and input to load into the stream. It then reads off of the stream using the parsec
-- functions, such as char, string, many, noneOf, etc
--readExpr input = case parse (spaces >> symbol) "lisp" input of
--    Left err -> "No match: " ++ show err
--    Right val -> "Found value"
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseQuoted
    <|> parseQuasiQuote
    <|> parseUnQuote
    <|> do char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x
    <|> try parseRatio
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseDec)
    char '+'
    y <- (try parseFloat <|> parseDec)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x

escapedChars :: Parser Char
escapedChars = do
    char '\\' -- a backslash
    x <- oneOf "\\\"nrt" -- either backslash or doublequote
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseChar :: Parser LispVal
parseChar = do try $ string "#\\"
               x <- try (string "newline" <|> string "space")
                    <|> do
                        x <- anyChar
                        notFollowedBy alphaNum
                        return [x]
               return $ Character $ case x of
                "space" -> ' '
                "newline" -> '\n'
                _ -> (x !! 0)

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
--parseNumber = do
--    x <- many1 digit
--    (return . Number . read) x
parseNumber = parseDec
    <|> parseDec2
    <|> parseHex
    <|> parseOct
    <|> parseBin

parseDec :: Parser LispVal
parseDec = many1 digit >>= (return . Number . read)

parseDec2 :: Parser LispVal
--TODO: Why do we use try here? Ans: Because string can look at more than one char, it can be destructive.
-- We don't need it with char, because char doesn't destroy the input if it fails to match
parseDec2 = do try $ string "#d"
--Whitespace? Ans: Has to line up with the grouped expression (try, in this case)
               x <- many1 digit
               (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)
hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)
oct2dig x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)
-- Recursive definition of binary nubmers. By default, we do bin2dig 0 so we start counting at 0
bin2dig = bin2dig' 0
-- Base case: Empty string is just our accumulator
bin2dig' digint "" = digint
-- Recursive case: Multiply the accum by 2 then add the current bit and recurse on the rest of the bitstring
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs