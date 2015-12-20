module Parser 
  ( 
   LispVal(..),
   run,
   parseExpr
  ) where
   
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec 
import System.Environment
import Control.Monad (liftM)
import Numeric

run p = parse p ""  

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf ['"'])
                char '"'
                return $ String x

{-
escapedChar :: Parser Char
escapedChar = do
  char '\\'
  x <- oneOf ['n', 't', 'r', '\\', '"']
  return $ case x of 
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    '"' -> x
    '\\' -> x
-}

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseSNum <|> liftM (Number . read) (many1 digit) where
    -- parse number in binary, octal, decimal and hex
  parseSNum = do 
    char '#' 
    parseBinary <|> parseOct <|> parseDec <|> parseHex
    
  parseBinary :: Parser LispVal
  parseBinary = do
    char 'b'
    num <- many1 (oneOf "01")
    return . Number $ foldl (\acc b -> acc * 2 + (if b == '0' then 0 else 1)) 0 num 

  parseOct :: Parser LispVal
  parseOct = do
    char 'o'
    num <- many1 octDigit
    return $ bind2first Number $ readOct num 

  parseDec :: Parser LispVal
  parseDec = do
    char 'd'
    num <- many1 digit
    return $ bind2first Number $ readDec num 

  parseHex :: Parser LispVal
  parseHex = do
    char 'x'
    num <- many1 hexDigit
    return $ bind2first Number $ readHex num 
    
parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  liftM Character (symbol <|> letter)

parseFloat :: Parser LispVal
parseFloat = do
  whole <- many1 digit
  p <- char '.'
  decimal <- many1 digit
  return . bind2first Float . readFloat $ whole ++ [p] ++ decimal

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
---------------------------
--helper 
--this function is not safe
bind2first :: (a -> c) -> [(a,b)] -> c
bind2first f (x:[]) = f . fst $ x
