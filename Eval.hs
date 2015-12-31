{-# LANGUAGE ExistentialQuantification #-}

module Eval 
  (
    eval
  ) where

import Parser
import Control.Monad.Error
import Text.ParserCombinators.Parsec 
import System.Environment

entrance :: String -> ThrowsError LispVal 
entrance str = (readExpr str) >>= eval

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
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List (Atom func : args)) =  mapM eval args >>= apply func  
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

----------------------------------------
--support list operation
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList
eqv :: [LispVal] -> ThrowsError LispVal

eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2 `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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
              ("string?", uniop isString),
	      ("=", numBoolBinop (==)),
	      ("<", numBoolBinop (<)),
	      (">", numBoolBinop (>)),
	      ("/=", numBoolBinop (/=)),
	      (">=", numBoolBinop (>=)),
	      ("<=", numBoolBinop (<=)),
	      ("&&", boolBoolBinop (&&)),
	      ("||", boolBoolBinop (||)),
	      ("string=?", strBoolBinop (==)),
	      ("string<?", strBoolBinop (<)),
	      ("string>?", strBoolBinop (>)),
	      ("string<=?", strBoolBinop (<=)),
	      ("string>=?", strBoolBinop (>=)),
	      ("car", car),
	      ("cdr", cdr),
	      ("cons", cons),
	      ("eq?", eqv),
	      ("eqv?", eqv),
	      ("equal?", equal)
             ]

-- function wrappers
uniop :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
uniop f [l] = return $ f l 
uniop _ xs = throwError $ NumArgs 1 xs

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum 

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop  = boolBinop unpackBool 

unpackBool :: LispVal -> ThrowsError Bool 
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "bool" notBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr 

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

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

