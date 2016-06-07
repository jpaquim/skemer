module Main where
import System.Environment
import System.IO
import Control.Monad.Error -- deprecated, replace with 
-- import Control.Monad.Except
import Numeric
import Data.Array
import Data.Complex
import Data.IORef
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (spaces)

-- Note: missing - load the standard library at start

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        else runOne args

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars
        [("args", List $ map String $ tail args)]
    (runIOThrows $ liftM show $
        eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl =
    primitiveBindings >>=
    until_ (== "quit") (readPrompt "Lisp >>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ condition prompt action = do
    result <- prompt
    if condition result
        then return ()
        else action result >> until_ condition prompt action

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
    return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef varBinds =
    readIORef envRef >>= extendEnv varBinds >>= newIORef
  where
    extendEnv varBindings env = liftM (++ env) (mapM addBinding varBindings)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)

data LispVal = Nil
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
--              deriving (Eq)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList beforeDot afterDot) = "(" ++ unwordsList beforeDot ++
    " . " ++ showVal afterDot ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func args varargs _ _) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++
        ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal val = show val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ (List [Atom "quote", val]) = return val
eval env (Atom ident) = getVar env ident

eval env (List [Atom "if", condition, conseq, alt]) = do
    result <- eval env condition
    case result of
        Bool False -> eval env alt
        Bool True  -> eval env conseq
        badArg -> throwError $ TypeMismatch "boolean" badArg

eval env form@(List (Atom "cond" : clauses)) = do
    if null clauses
        then throwError $ BadSpecialForm
            "no true clause in cond expression: " form
        else case head clauses of
            List [Atom "else", expr] -> eval env expr
            List [test, expr]        -> eval env $ List [Atom "if", test, expr,
                List (Atom "cond" : tail clauses)]
            _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form

-- Note: needs to be updated for IOThrowsError
-- eval env form@(List (Atom "case" : key : clauses)) = do
--     if null clauses
--         then throwError $ BadSpecialForm
--             "no true clause in case expression: " form
--         else case head clauses of
--             List (Atom "else" : exprs) -> mapM (eval env) exprs >>=
--                 return . last
--             List ((List datums) : exprs) -> do
--                 result <- eval env key
--                 equality <- mapM (\x -> eqv [result, x]) datums
--                 if Bool True `elem` equality
--                     then mapM (eval env) exprs >>= return . last
--                     else eval env $ List (Atom "case" : key : tail clauses)
--             _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : funcParams) : funcBody)) =
    makeNormalFunc env funcParams funcBody >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : funcParams)
  varargs : funcBody)) =
    makeVarArgs varargs env funcParams funcBody >>= defineVar env var
eval env (List (Atom "lambda" : List funcParams : funcBody)) =
    makeNormalFunc env funcParams funcBody
eval env (List (Atom "lambda" : DottedList funcParams varargs : funcBody)) =
    makeVarArgs varargs env funcParams funcBody
eval env (List (Atom "lambda" : varargs@(Atom _) : funcBody)) =
    makeVarArgs varargs env [] funcBody
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func funcParams varargs funcBody funcClosure) args =
    if num funcParams /= num args && varargs == Nothing
        then throwError $ NumArgs (num funcParams) args
        else (liftIO $ bindVars funcClosure $ zip funcParams args) >>=
            bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length funcParams) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) funcBody
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env
apply (IOFunc func) args = func args
apply _ _ = undefined

primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= (flip bindVars $ map (makeFunction IOFunc) ioPrimitives
                              ++ map (makeFunction PrimitiveFunc) primitives)
  where makeFunction constructor (var, func) = (var, constructor func)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] ->
    IOThrowsError LispVal
makeFunc varargs env funcParams funcBody =
    return $ Func (map showVal funcParams) varargs funcBody env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              ("<=", numBoolBinop (<=)),
              (">=", numBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", boolUnop isSymbol),
              ("string?", boolUnop isString),
              ("number?", boolUnop isNumber),
              ("boolean?", boolUnop isBool),
              ("list?", boolUnop isList),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", eqv)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->
    ThrowsError LispVal
numericBinop _         [] = throwError $ NumArgs 2 []
numericBinop _ oneArg@[_] = throwError $ NumArgs 2 oneArg
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1 op
-- numericBinop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] ->
    ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                 then throwError $ NumArgs 2 args
                                 else do
                                     left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right
numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
-- convert numbers and bools to their string equivalent
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

boolUnop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
boolUnop op [param]  = return $ Bool $ op param
boolUnop _ notSingle = throwError $ NumArgs 1 notSingle

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _ = False

isList :: LispVal -> Bool
isList (List _) = True
isList _ = False

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol [notString] = throwError $ TypeMismatch "string" notString
stringToSymbol notSingle = throwError $ NumArgs 1 notSingle

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbolToString notSingle = throwError $ NumArgs 1 notSingle

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

-- Note: in actual implementations, eq?, eqv? and equal? are different,
-- namely only equal is guaranteed to be recursive for lists
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) &&
        (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
        Left _ -> False
        Right (Bool val) -> val
        _ -> undefined
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


--- IO functionality ---

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [_]              = writeProc [Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


--- Exception handling ---

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = undefined

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (NumArgs expected found) = 
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Default message) = message


--- Parser ---

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
    case parse parser "lisp" input of
        Left err  -> throwError $ Parser err
        Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> try parseComplex -- must come before parseNumber and parseFloat
         <|> try parseFloat   -- must come before parseNumber
         <|> try parseRatio   -- must come before parseNumber
         <|> try parseNumber  -- we need the 'try' because 
         <|> try parseBool    -- these can all start with the hash char
         <|> try parseCharacter
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> try parseVector
         <|> parseList

parseList :: Parser LispVal
parseList = do
    _ <- char '(' >> optionalSpaces
    beforeDot <- sepEndBy parseExpr spaces
    afterDot <- option Nil (char '.' >> spaces >> parseExpr)
    _ <- optionalSpaces >> char ')'
    return $ case afterDot of
        Nil -> List beforeDot
        _   -> DottedList beforeDot afterDot

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= \x ->
              return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = char '`' >> parseExpr >>= \x ->
                   return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = char ',' >> parseExpr >>= \x ->
               return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
    _ <- string "#("
    arrayValues <- sepBy parseExpr spaces
    _ <- char ')'
    return $ Vector $ listArray (0, (length arrayValues - 1)) arrayValues

parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first:rest

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- string "#\\"
    value <- try (string "newline" <|> string "space")
         <|> (anyChar >>= \x -> notFollowedBy alphaNum >> return [x])
    return $ Character $ case value of
        "space"   -> ' '
        "newline" -> '\n'
        _         -> value !! 0

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (escapedChars <|> noneOf "\"\\")
    _ <- char '"'
    return $ String x

parseNumber :: Parser LispVal
parseNumber =  parseDec <|> try parseDec' <|> try parseHex <|> try parseOct
           <|> try parseBin

parseDec :: Parser LispVal
parseDec = many1 digit >>= return . Number . read

parseDec' :: Parser LispVal
parseDec' = do
    _ <- string "#d"
    x <- many1 digit
    return $ Number $ read x

parseHex :: Parser LispVal
parseHex = do
    _ <- string "#x"
    x <- many1 hexDigit
    return $ Number $ fst $ head $ readHex x

parseOct :: Parser LispVal
parseOct = do
    _ <- string "#o"
    x <- many1 octDigit
    return $ Number $ fst $ head $ readOct x

parseBin :: Parser LispVal
parseBin = do
    _ <- string "#b"
    x <- many1 $ oneOf "10"
    return $ Number $ foldl (\s y -> s * 2 + (if y == '0' then 0 else 1)) 0 x

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    _ <- char '.'
    y <- many1 digit
    return $ Float $ fst $ head $ readFloat $ x ++ "." ++ y

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    _ <- char '/'
    y <- many1 digit
    return $ Ratio $ read x % read y

parseComplex :: Parser LispVal
parseComplex = do
    x <- try parseFloat <|> parseDec
    _ <- char '+'
    y <- try parseFloat <|> parseDec
    _ <- char 'i'
    return $ Complex $ toDouble x :+ toDouble y

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n
toDouble (_) = 0.0 -- to avoid warnings

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

optionalSpaces :: Parser ()
optionalSpaces = skipMany space

escapedChars :: Parser Char
escapedChars = do
    _ <- char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
                 '\\' -> x
                 '"' -> x
                 'n' -> '\n'
                 'r' -> '\r'
                 _   -> '\t'

-- parseParenList :: Parser LispVal
-- parseParenList = char '(' >> (try parseList <|> parseDottedList) >>= \x ->
--                  char ')' >> return x

-- parseList :: Parser LispVal
-- parseList = sepBy parseExpr spaces >>= (return . List)
-- 
-- parseDottedList :: Parser LispVal
-- parseDottedList = do
--     beforeDot <- endBy parseExpr spaces
--     afterDot <- char '.' >> spaces >> parseExpr
--     return $ DottedList beforeDot afterDot

-- allow for lists and strings that can be parsed as numbers to be interpreted
-- as such
-- unpackNum :: LispVal -> Integer
-- unpackNum (Number n) = n
-- unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
--                            if null parsed
--                               then 0
--                               else fst $ head $ parsed
-- unpackNum (List [n]) = unpackNum n
-- unpackNum _ = 0
