module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment

data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool

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
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--TODO: How does this typing work?
spaces :: Parser ()
spaces = skipMany1 space

--vowel :: Parser Char
--vowel = oneOf "aeiou"

readExpr :: String -> String
--TODO: How does parse work? look at types
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
--TODO: everything after import Control.Monad
parseNumber = liftM (Number . read) $ many1 digit