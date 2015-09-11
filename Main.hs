module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Numeric
import Data.Ratio

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
        -- | Complex Complex

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
    --name <- getLine
    --putStrLn(name)
    --args <- getArgs
    --putStrLn("Hello, " ++ args !! 0 ++ " and " ++ args !! 1)
    --putStrLn("Result is " ++ show(read(args !! 0) + read(args !! 1)))

--TODO: How does this typing work?
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--TODO: How does this typing work?
spaces :: Parser ()
spaces = skipMany1 space

--vowel :: Parser Char
--vowel = oneOf "aeiou"

readExpr :: String -> String
--TODO: How does parse work? look at types ans:
-- parse takes an expresion to match against, a (seemingly arbitrary) name to represent this
-- value, and input to load into the stream. It then reads off of the stream using the parsec
-- functions, such as char, string, many, noneOf, etc
--readExpr input = case parse (spaces >> symbol) "lisp" input of
--    Left err -> "No match: " ++ show err
--    Right val -> "Found value"
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> try parseRatio
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar

--TODO: Finish me
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
----TODO: match space or newline. Also use anyChar, and notFollowedBy alphaNum to match only one
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

