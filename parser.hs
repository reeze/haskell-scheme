{-# LANGUAGE ExistentialQuantification #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint "" = putStrLn ""
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
	result <- prompt
	if pred result
		then return ()
		else action result >> until_ pred prompt action


runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

spaces :: Parser ()
spaces = skipMany1 space 


data LispVal = Atom String
			 | List [LispVal]
			 | DottedList [LispVal] LispVal
			 | Number Integer
			 | String String
			 | Nil
			 | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
			   | Parser ParseError
			   | BadSpecialForm String LispVal
			   | NotFunction String String
			   | UnboundVar String String
			   | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ showType found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


showType :: LispVal -> String
showType val = case val of
	(Atom str) -> "atom"
	(List lst) -> "list" 
	(DottedList dlst v) -> "dlist"
	(Number num) -> "number"
	(String str) -> "string" 
	(Nil) -> "nil"
	(Bool b) -> "boolean"
	other -> "unknown"

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


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
		_    -> Atom atom

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
	str <- many1 digit
	return $ (Number . read) str

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
         <|> do
		 	char '('
		 	x <- try parseList <|> parseDottedList
			char ')'
			return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Nil) = "Nil"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, yescond, nocond]) =
                                               do result <- eval pred
                                                  case result of
                                                       Bool False -> eval nocond
                                                       Bool True -> eval yescond
                                                       other -> throwError $ TypeMismatch "Pred must be boolean" other
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecogizaed special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecogizaed primitive function args" func)
                        ($ args)
						(lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
			  ("*", numericBinop (*)),
			  ("/", numericBinop div),
			  ("mod", numericBinop mod),
			  ("quotient", numericBinop quot),
			  ("=", numBoolBinop (==)),
			  ("<", numBoolBinop (<)),
			  (">", numBoolBinop (>)),
			  ("/=", numBoolBinop (/=)),
			  (">=", numBoolBinop (>=)),
			  ("<=", numBoolBinop (<=)),
			  ("&&", boolBoolBinop(&&)),
			  ("||", boolBoolBinop(||)),
			  ("car", car),
			  ("cdr", cdr),
			  ("cons", cons),
			  ("eq?", eq),
			  ("eqv?", eq),
			  ("equal?", equal),
			  ("string=?", strBoolBinop(==)),
			  ("string<?", strBoolBinop(<)),
			  ("string>?", strBoolBinop(<)),
			  ("string<=?", strBoolBinop(<=)),
			  ("string>=?", strBoolBinop(>=)),
			  ("remainder", numericBinop rem)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op            [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- It is strict 2 operator by reeze
-- numericBinop op all@(x:y:z:xs) = throwError $ NumArgs 2 all
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op


numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
							if null parsed
								then throwError $ TypeMismatch "number" $ String n
								else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg]    = throwError $ TypeMismatch "pair" badArg
car badArgList  = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]           = return $ List xs
cdr [List (x)]              = return Nil
cdr [DottedList [_] x]      = return x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x1, List xs]            = return $ List $ x1 : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList


eq ::[LispVal] -> ThrowsError LispVal
eq [(Bool x1), (Bool x2)]        = return $ Bool $ x1 == x2
eq [(Number x1), (Number x2)]    = return $ Bool $ x1 == x2
eq [(Atom x1), (Atom x2)]        = return $ Bool $ x1 == x2
eq [(String x1), (String x2)]    = return $ Bool $ x1 == x2
eq [(List x1), (List x2)]        = return $ Bool $ (length x1 == length x2) && (all eqvPair $ zip x1 x2)
	where eqvPair (l, r)         = case eq [l, r] of
		Left err -> False
		Right (Bool val) -> val
eq [_, _]                        = return $ Bool False
eq badArgList                    = throwError $ NumArgs 2 badArgList



-- equal?
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return  $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List x1), (List x2)] = return $ Bool $ (length x1 == length x2) && (all eqvPair $ zip x1 x2)
	where eqvPair (l, r) = case equal [l, r] of
		Left err -> False
		Right (Bool val) -> val
equal [x1, x2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals x1 x2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackBool, AnyUnpacker unpackStr]
  eqEquals <- eq [x1, x2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

main :: IO ()
main = do
	args <- getArgs
	case length args of
		0 -> runRepl
		1 -> evalAndPrint $ args !! 0
		otherwise -> putStrLn "Programm only take 0 or one arg"
