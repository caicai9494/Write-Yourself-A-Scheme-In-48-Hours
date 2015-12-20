module Eval 
  (
    eval
  ) where

import Parser
import Control.Monad.Error
import Text.ParserCombinators.Parsec 

instance Show LispVal where
  show = showVal where

    showVal :: LispVal -> String
    showVal (String contents) = "\"" ++ contents ++ "\""
    showVal (Atom name) = name
    showVal (Number contents) = show contents
    showVal (Bool True) = "#t"
    showVal (Bool False) = "#f"
    showVal (List contents) = "(" ++ unwordsList contents ++ ")"
    showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

    unwordsList :: [LispVal] -> String
    unwordsList = unwords. map showVal 

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) =  mapM eval args >>= apply func  
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", uniop isNumber),
              ("string?", uniop isString)
             ]

uniop :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
uniop f [l] = return $ f l 
uniop _ xs = throwError $ NumArgs 1 xs

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op where

    unpackNum :: LispVal -> ThrowsError Integer
    unpackNum (Number n) = return n
    unpackNum val@(String n) = let parsed = reads n :: [(Integer, String)] in 
			       if null parsed 
				  then throwError $ TypeMismatch "string" val
				  else return $ fst $ parsed !! 0
    unpackNum (List [n]) = unpackNum n
    unpackNum notNum = throwError $ TypeMismatch "number" notNum

------------------------
--type checkers
isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False
------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


instance Show LispError where 
  show = showError where

    showError :: LispError -> String
    showError (UnboundVar message varname)  = message ++ ": " ++ varname
    showError (BadSpecialForm message form) = message ++ ": " ++ show form
    showError (NotFunction message func)    = message ++ ": " ++ show func
    showError (NumArgs expected found)      = "Expected " ++ show expected 
					   ++ " args; found values " ++ unwordsList found
    showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
					   ++ ", found " ++ show found
    showError (Parser parseErr)             = "Parse error at " ++ show parseErr 

    unwordsList :: [LispVal] -> String
    unwordsList = unwords. map show 

 

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

-- must be called after trapError
-- fail this precondition is broken
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = undefined -- represents a programmer error

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val
