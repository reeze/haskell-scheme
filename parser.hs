{-# LANGUAGE ExistentialQuantification #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

-- Env
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
					         (liftIO . readIORef)
						     (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting and unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
	alreadDefined <- liftIO $ isBound envRef var
	if alreadDefined
		then setVar envRef var value >> return value
		else liftIO $ do
			valueRef <- newIORef value
			env <- readIORef envRef
			writeIORef envRef ((var, valueRef) : env)
			return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do ref <- newIORef value
                                 return (var, ref)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

-- REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env "" = putStrLn ""
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
	result <- prompt
	if pred result
		then return ()
		else action result >> until_ pred prompt action

spaces :: Parser ()
spaces = skipMany1 space 


data LispVal = Atom String
			 | List [LispVal]
			 | DottedList [LispVal] LispVal
			 | Number Integer
			 | String String
			 | Nil
			 | Bool Bool
			 | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
			 | Func { params :: [String], vararg :: (Maybe String),
			          body :: [LispVal], closure :: Env }

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

instance Error LispError  where 
	noMsg = Default "An error occurred"
	strMsg = Default

showType :: LispVal -> String
showType val = case val of
	(Atom str) -> "atom"
	(List lst) -> "list" 
	(DottedList dlst v) -> "dlist"
	(Number num) -> "number"
	(String str) -> "string" 
	(Nil) -> "nil"
	(Bool b) -> "boolean"

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
	"(lambda (" ++ unwords (map show args) ++
		(case varargs of
			Nothing -> ""
			Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, yescond, nocond]) =
                                               do result <- eval env pred
                                                  case result of
                                                       Bool False -> eval env nocond
                                                       Bool True -> eval env yescond
                                                       other -> throwError $ TypeMismatch "Pred must be boolean" other
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
	makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
	makeVarArgs varargs env [] body
eval env (List (function : args)) = do
	func <- eval env function
	argVals <- mapM (eval env) args
	apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecogizaed special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func	args
apply (Func params varargs body closure) args = 
	if num params /= num args && varargs == Nothing
		then throwError $ NumArgs (num params) args
		else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
	where
		remainingArgs = drop (length params) args
		num = toInteger . length
		evalBody env = liftM last $ mapM (eval env) body
		bindVarArgs arg env = case arg of
			Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
			Nothing -> return env
		

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
	where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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
		1 -> runOne $ args !! 0
		otherwise -> putStrLn "Programm only take 0 or one arg"
