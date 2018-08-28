{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}

module Parser where

import           Control.Arrow                  ( app )
import           Control.Applicative            ( Const(..)
                                                , liftA2
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Except
import           Data.Void
import           Control.Monad.State.Strict     ( get
                                                , put
                                                )
import           Control.Monad.Writer.Strict    ( MonadWriter(..)
                                                , WriterT(..)
                                                , listen
                                                , runWriterT
                                                , tell
                                                )
import qualified Data.List.Split               as Spl
import           Data.Function
import           Text.Megaparsec                ( (<|>)
                                                , (<?>)
                                                )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L
import           System.FilePath                ( addTrailingPathSeparator
                                                , splitFileName
                                                , (</>)
                                                )
import           System.IO.Error                ( tryIOError )
import qualified System.Directory              as Dir
                                                ( makeAbsolute )
import qualified Data.Map.Strict               as Map
import           Data.Functor

import           IState
import           Ast
import           FileUtils
import           Helper

type Parsing m = (P.MonadParsec Void String m, MonadWriter FC m)

unit :: CbParser Unit
unit = Unit <$> P.many declaration

import_ :: CbParser Import
import_ = rword "import" *> do
  ist      <- get
  (ns, fc) <- withExtent moduleNamePieces <* lchar ';'
  return $ Import fc (toPath $ projectDriectory ist : ns)
 where
  moduleNamePieces = Spl.splitOn "." <$> identifier
  toPath           = foldl1 (</>)

imports_ :: CbParser (Maybe Mark, [Import])
imports_ = do
  ps_list <- P.many import_
  mrk     <- mark
  isEof   <- lookAheadMatches P.eof
  let mrk' = if isEof then Nothing else Just mrk
  return (mrk', ps_list)

---------------------------- scope --------------------------------------

pushScope :: CbParser ()
pushScope = do
  ist <- get
  put $ ist { scope = Scope {parent = Just $ scope ist, decls = Map.fromList []}
            }

insertDeclToScope :: String -> Declaration -> CbParser ()
insertDeclToScope name decl = do
  ist <- get
  let oldDecls = decls $ scope ist
  let oldScope = scope ist
  put $ ist { scope = oldScope { decls = Map.insert name decl oldDecls } }

pushDeclToScope :: Declaration -> CbParser ()
pushDeclToScope decl@(UndefineFunction _ _ funcName _ _) = do
  ist <- get
  case searchByScope funcName ist of
    Nothing         -> insertDeclToScope funcName decl
    Just Function{} -> pure ()
    _               -> fail "存在重复声明"
pushDeclToScope decl@(Function _ _ funcName _ _ _) = do
  ist <- get
  case searchByScope funcName ist of
    Nothing                 -> insertDeclToScope funcName decl
    Just UndefineFunction{} -> insertDeclToScope funcName decl
    _                       -> fail "存在重复声明"
pushDeclToScope decl = do
  ist <- get
  let name = declName decl
  case searchByScope name ist of
    Just _  -> fail "存在重复声明"
    Nothing -> insertDeclToScope name decl


---------------------------- expr -----------------------------------------

primary :: CbParser Expr
primary = CharLiteral <$> getFC <*> charLiteral
      <|> StringLiteral <$> getFC <*> stringLiteral
      <|> BoolLiteral <$> getFC <*> bool
      <|> P.try (FloatLiteral <$> getFC <*> float)
      <|> IntLiteral <$> getFC <*> natural
      <|> decl
      <|> lchar '(' *>  expr' <*  lchar ')'
 where
  decl = do
    ist  <- get
    name <- identifier
    fc   <- getFC
    case searchByScope name ist of
      Just decl -> pure $ Decl fc name $ declId decl
      _         -> fail "match var fail"

orOp :: Parsing m => [String] -> m String
orOp ls = foldr1 (<|>) (map lstring ls)

term :: CbParser Expr
term = P.try cast <|> P.try unary <|> primary
 where
  postfix' p op =
    (do
        f <- op
        postfix' (f p) op
      )
      <|> pure p
  postfixOps =
    Funcall
      <$> getFC
      <*> P.try (lchar '(' *> P.sepBy expr (lchar ',') <* lchar ')')
      <|> Suffix
      <$> getFC
      <*> orOp ["++", "--"]
      <|> Arrayref
      <$> getFC
      <*> P.try (lchar '[' *> expr <* lchar ']')
      <|> Member
      <$> getFC
      <*> (lchar '.' *> identifier)
      <|> PtrMember
      <$> getFC
      <*> (lstring "->" *> identifier)
  postfix = primary >>= \p -> postfix' p postfixOps
  cast    = Cast <$> getFC <*> P.try (lchar '(' *> type_) <* lchar ')' <*> expr
  unary =
    Prefix
      <$> getFC
      <*> orOp ["++", "--"]
      <*> unary
      <|> Unary
      <$> getFC
      <*> orOp ["+", "-", "!", "~"]
      <*> unary
      <|> lchar '*'
      *>  (Dereference <$> getFC <*> term)
      <|> lchar '&'
      *>  (Address <$> getFC <*> term)
      <|> rword "sizeof"
      *>  (   SizeofType
          <$> getFC
          <*> P.try (lchar '(' *> type_)
          <*  lchar ')'
          <|> SizeofExpr
          <$> getFC
          <*> unary
          )
      <|> postfix

expr' :: CbParser Expr
expr' = P.sepBy1 expr (lchar ',') >>= \es ->
  if length es == 1 then pure $ head es else Seq <$> getFC <*> pure es

expr :: CbParser Expr
expr = expr_assign <|> expr_opassign <|> expr10
 where
  binary p op = scan
   where
    scan = do
      l <- p
      rest l <|> return l
    rest l = do
      f <- op
      f l <$> scan
  op ls = Binary <$> getFC <*> orOp ls
  opassign_op = rword "+=" $>  "+"
            <|> rword "-=" $>  "-"
            <|> rword "*=" $>  "*"
            <|> rword "/=" $>  "/"
            <|> rword "%=" $>  "%"
            <|> rword "&=" $>  "&"
            <|> rword "|=" $>  "|"
            <|> rword "^=" $>  "^"
            <|> rword "<<=" $>  "<<"
            <|> rword ">>=" *>  pure ">>"
            <?> "opassign_op"
  expr1 = binary term (op ["*", "/", "%"])
  expr2 = binary expr1 (op ["+", "-"])
  expr3 = binary expr2 (op [">>", "<<"])
  expr4 = binary expr3 (op ["&"])
  expr5 = binary expr4 (op ["^"])
  expr6 = binary expr5 (op ["|"])
  expr7 = binary expr6 (op [">=", "<=", ">", "<", "==", "!="])
  expr8 = binary expr7 (op ["and"])
  expr9 = binary expr8 (op ["or"])
  expr10 =
    P.try
        (   Cond
        <$> getFC
        <*> expr9
        <*> (lchar '?' *> expr)
        <*> (lchar ':' *> expr10)
        )
      <|> expr9
  expr_assign = do
    fc  <- getFC
    lhs <- P.try (term <* lchar '=')
    Assign fc lhs <$> expr
  expr_opassign = do
    fc        <- getFC
    (lhs, op) <- P.try ((,) <$> term <*> opassign_op)
    OpAssign fc op lhs <$> expr

---------------------- stmt ------------------------------


stmt :: CbParser Stmt
stmt =
  rword "goto"
    *>  (Goto <$> getFC <*> identifier)
    <*  lchar ';'
    <|> rword "break"
    *>  (Break <$> getFC)
    <*  lchar ';'
    <|> rword "continue"
    *>  (Continue <$> getFC)
    <*  lchar ';'
    <|> rword "return"
    *>  (Return <$> getFC <*> P.optional expr)
    <*  lchar ';'
    <|> Expression
    <$> getFC
    <*> expr
    <*  lchar ';'
    <|> if_
    <|> while_
    <|> switch_
    <|> for_
    <|> lchar '{'
    *>  block_ True
    <*  lchar '}'
    <|> Label
    <$> getFC
    <*> identifier
    <*  lchar ':'

panes s = rword s *> lchar '(' *> expr <* lchar ')'

if_ :: CbParser Stmt
if_ =
  If <$> getFC <*> panes "if" <*> stmt <*> P.optional (rword "else" *> stmt)

switch_ :: CbParser Stmt
switch_ =
  Switch
    <$> getFC
    <*> panes "switch"
    <*  lchar '{'
    <*> P.many case_
    <*> P.optional default_
    <*  lchar '}'
 where
  caseblock = lchar '{' *> block_ True <* lchar '}' <|> block_ False
  case_ = Case <$> getFC <*> (rword "case" *> expr <* lchar ':') <*> caseblock
  default_ = Default <$> getFC <*> (rword "default" *> lchar ':' *> caseblock)

while_ :: CbParser Stmt
while_ =
  While
    <$> getFC
    <*> panes "while"
    <*> stmt
    <|> DoWhile
    <$> getFC
    <*> (rword "do" *> stmt)
    <*> panes "while"

for_ :: CbParser Stmt
for_ =
  For
    <$> getFC
    <*  rword "for"
    <*  lchar '('
    <*> P.optional expr'
    <*  lchar ';'
    <*> P.optional expr
    <*  lchar ';'
    <*> P.optional expr'
    <*  lchar ')'
    <*  lchar '{'
    <*> block_ True
    <*  lchar '}'

block' :: Bool -> [Declaration] -> CbParser Stmt
block' b decls = do
  ist <- get
  when b pushScope
  forM_ decls pushDeclToScope
  res <- P.many (P.try (Left <$> defvar') <|> (Right <$> stmt)) >>= block_stmt'
  nst <- get
  when b (put $ nst { scope = scope ist })
  return res
 where
  defvar' = defvarNoParam >>= \d -> do pushDeclToScope d; pure d
  block_stmt' = block (decls, [])
  block (vs, ss) (Left  v  : es) = block (v : vs, ss) es
  block (vs, ss) (Right st : es) = block (vs, st : ss) es
  block (vs, ss) []              = Block <$> getFC <*> pure vs <*> pure ss
  getDecls (Block _ decls _) = decls

block_ :: Bool -> CbParser Stmt
block_ b = block' b []

---------------------------- decl ----------------------------------
declIdGen :: CbParser Int
declIdGen = do
  ist <- get
  let id = declCount ist + 1
  put $ ist { declCount = id }
  pure id

declHandlerGen :: Declaration -> CbParser ()
declHandlerGen decl = do
  ist <- get
  put $ ist { handlers = Map.insert (declId decl) decl (handlers ist) }

declaration :: CbParser Declaration
declaration =
  defsturt
    <|> defunion
    <|> typedef
    <|> P.try declfun'
    <|> P.try defun
    <|> defvarNoParam
    >>= \decl -> do
          pushDeclToScope decl
          declHandlerGen decl
          pure decl

declarationByImport :: CbParser Declaration
declarationByImport =
  defsturt <|> defunion <|> typedef <|> P.try declfun' <|> defvarNoParam >>= \decl ->
    do
      pushDeclToScope decl
      declHandlerGen decl
      pure decl

defvarNoParam :: CbParser Declaration
defvarNoParam = defvarSig <* lchar ';'

defvarSig :: CbParser Declaration
defvarSig = do
  var <- Variable <$> getFC <*> type_ <*> identifier <*> declIdGen
  declHandlerGen var
  pure var

declfun :: CbParser Declaration
declfun =
  UndefineFunction
    <$> getFC
    <*> type_
    <*> identifier
    <* lchar '(' <*> params <* lchar ')'
    <*> declIdGen
  where params = P.sepEndBy defvarSig $ lchar ','

declfun' :: CbParser Declaration
declfun' = declfun <* lchar ';'

defun :: CbParser Declaration
defun = declfun >>= defun'
 where
  defun' (UndefineFunction fc t name params id) =
    Function fc t name params
      <$> (lchar '{' *> block' True params <* lchar '}')
      <*> declIdGen

addType :: Declaration -> CbParser Declaration
addType decl = do
  ist <- get
  let name  = declName decl
  let type_ = declToType decl
  case Map.lookup name (types ist) of
    Just _  -> fail "试图覆盖声明一个已有的类型定义"
    Nothing -> put $ ist { types = Map.insert name type_ (types ist) }
  return decl

defsturt :: CbParser Declaration
defsturt =
  rword "struct"
    *>  (   Struct
        <$> getFC
        <*> identifier
        <*> (lchar '{' *> slots ';' <* lchar '}')
        )
    <*  lchar ';'
    <*> declIdGen
    >>= addType

defunion :: CbParser Declaration
defunion =
  rword "union"
    *>  (   Union
        <$> getFC
        <*> identifier
        <*> (lchar '{' *> slots ';' <* lchar '}')
        )
    <*  lchar ';'
    <*> declIdGen
    >>= addType

typedef :: CbParser Declaration
typedef =
  rword "typedef"
    *>  (Typedef <$> getFC <*> type_ <*> identifier)
    <*  lchar ';'
    <*> declIdGen
    >>= addType

slots :: Char -> CbParser [Slot]
slots c = P.sepEndBy slot (lchar c)
 where
  slot = Slot <$> type_ <*> identifier

------------------------------- type -----------------------------------

-- type 包含了基本类型与自定义类型，其中自定义类型在parser阶段全部设定为 CbUnknown
type_ :: CbParser Type
type_ =
  P.try (CbPtr <$> typeSign <* lchar '*')
    <|> P.try (CbPtr <$> typeSign <* lchar '[' <* lchar ']')
    <|> P.try (CbArray <$> typeSign <* lchar '[' <*> natural <* lchar ']')
    <|> P.try (CbConst <$> (rword "const" *> type_))
    <|> typeSign
 where
  typeSign =
    rword "int"
      $>  CbInt
      <|> rword "bool"
      $>  CbBool
      <|> rword "char"
      $>  CbChar
      <|> rword "void"
      $>  CbVoid
      <|> rword "long"
      $>  CbLong
      <|> rword "float"
      $>  CbFloat
      <|> rword "double"
      $>  CbDouble
      <|> P.try (rword "unsigned" *> rword "int")
      $>  CbUInt
      <|> P.try (rword "unsigned" *> rword "char")
      $>  CbChar
      <|> rword "unsigned"
      *>  rword "long"
      $>  CbULong
      <|> CbUnknown
      <$> identifier

--------------------------- base -------------------------------------------

mark :: Parsing m => m Mark
mark = P.getParserState

restore :: Parsing m => Mark -> m ()
restore = P.setParserState

lookAheadMatches :: Parsing m => m a -> m Bool
lookAheadMatches p = isJust <$> P.lookAhead (P.optional p)
 where
  isJust (Just _) = True
  isJust _        = False

sourcePositionFC :: P.SourcePos -> FC
sourcePositionFC (P.SourcePos name line column) = FC
  f
  (lineNumber, columnNumber)
  (lineNumber, columnNumber)
 where
  lineNumber      = P.unPos line
  columnNumber    = P.unPos column
  (dir, filename) = splitFileName name
  f = if dir == addTrailingPathSeparator "." then filename else name

getFC :: Parsing m => m FC
getFC = sourcePositionFC <$> P.getPosition

addExtent :: MonadWriter FC m => FC -> m ()
addExtent = tell

trackExtent :: Parsing m => m a -> m a
trackExtent p = do
  fc                <- getFC
  (FC f (sr, sc) _) <- getFC
  result            <- p
  (FC f _ (er, ec)) <- getFC
  addExtent (FC f (sr, sc) (er, max 1 (ec - 1)))
  return result

withExtent :: MonadWriter FC m => m a -> m (a, FC)
withExtent = listen

extent :: MonadWriter FC m => m a -> m FC
extent = fmap snd . withExtent

------------------------------------------------------------------

sc :: Parsing m => m ()
sc = L.space P.space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parsing m => m a -> m a
lexeme p = trackExtent $ L.lexeme sc p

symbol :: Parsing m => String -> m String
symbol = L.symbol sc

char :: Parsing m => Char -> m Char
char = P.char

lchar :: Parsing m => Char -> m Char
lchar = lexeme . P.char

string :: Parsing m => String -> m String
string = P.string

lstring :: Parsing m => String -> m String
lstring s = lexeme $ P.string s

charLiteral :: Parsing m => m Char
charLiteral = lexeme $ P.char '\'' *> L.charLiteral <* P.char '\''

stringLiteral :: Parsing m => m String
stringLiteral = lexeme $ P.char '"' *> P.many L.charLiteral <* P.char '"'

float :: Parsing m => m Double
float = lexeme L.float

natural :: Parsing m => m Int
natural = lexeme
  (   P.try (P.char '0' *> P.char' 'x' *> L.hexadecimal)
  <|> P.try (P.char '0' *> P.char' 'o' *> L.octal)
  <|> P.try L.decimal
  )

bool :: Parsing m => m Bool
bool = rword "true" $> True <|> rword "false" $> False

semi :: Parsing m => m String
semi = symbol ";"

equal :: Parsing m => m String
equal = symbol "="

rws :: [String]
rws =
  [ "void"
  , "char"
  , "short"
  , "int"
  , "long"
  , "float"
  , "double"
  , "struct"
  , "union"
  , "enum"
  , "static"
  , "extern"
  , "const"
  , "signed"
  , "unsigned"
  , "if"
  , "else"
  , "switch"
  , "case"
  , "default"
  , "while"
  , "do"
  , "for"
  , "return"
  , "break"
  , "continue"
  , "goto"
  , "typedef"
  , "import"
  , "sizeof"
  , "and"
  , "or"
  ]

rword :: Parsing m => String -> m ()
rword w =
  lexeme (P.string w *> P.notFollowedBy (P.alphaNumChar <|> P.char '_'))

identifier :: Parsing m => m String
identifier = (lexeme . P.try) (p >>= check)
 where
  p = (:) <$> P.letterChar <*> P.many P.alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x
