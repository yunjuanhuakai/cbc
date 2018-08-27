{-# LANGUAGE DeriveGeneric #-}

module IR where

import qualified Ast                           as A
import qualified Data.Vector                   as V
import qualified Data.Set                      as S
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           GHC.Generics                   ( Generic )
import           Text.PrettyPrint.GenericPretty ( Out )

type Stmts = V.Vector Stmt

data IR = IR
        {
          irName :: String
        , retType :: Type
        , params :: [Expr]
        , stmts :: V.Vector Stmt
        } 
        deriving (Eq, Ord)

instance Show IR where
  show (IR name rt params stmts) = "fun " ++  name ++ "(" ++ paramsStr ++  "): " ++ show rt ++ " {\n" ++ stmtsStr ++ "}"
    where paramsStr = intercalate ", " $ fmap show params
          stmtsStr = intercalate "\n" $ fmap show $ V.toList stmts

data Type = I8
          | I16
          | I32
          | I64
          | UI8
          | UI16
          | UI32
          | UI64
          | F32
          | F64
          deriving (Show, Eq, Ord, Generic)

transformType :: A.Type -> Type
transformType (A.CbConst t) = transformType t
transformType A.CbPtr{}     = UI64
transformType A.CbArray{}   = UI64
transformType A.CbLong      = I64
transformType A.CbULong     = UI64
transformType A.CbInt       = I32
transformType A.CbUInt      = UI32
transformType A.CbFloat     = F32
transformType A.CbDouble    = F64
transformType A.CbChar      = I8
transformType A.CbUChar     = UI8
transformType A.CbBool      = I8
transformType A.CbVoid      = I8

data Stmt = Assign Expr Expr
          | CJump Expr Stmt Stmt
          | Jump Stmt
          | Label String
          | Expression Expr
          | Return (Maybe Expr)
          deriving (Eq, Ord, Generic)

stmtToString :: Stmt -> String
stmtToString (Assign l r) = show l ++ " <- " ++ show r
stmtToString (CJump c l1 l2) = "if (" ++ show c ++ ") then "
                ++ stmtToString l1
                ++ " else "
                ++ stmtToString l2
stmtToString (Jump       l       ) = "goto " ++ stmtToString l
stmtToString (Label      name    ) = name
stmtToString (Expression e       ) = show e
stmtToString (Return     (Just e)) = "ret " ++ show e
stmtToString (Return     Nothing ) = "ret"

instance Show Stmt where
  show s@Label{} = stmtToString s ++ ":"
  show s = "\t" ++ stmtToString s

isAssign :: Stmt -> Bool
isAssign Assign{} = True
isAssign _        = False

assignLv :: Stmt -> Maybe Expr
assignLv (Assign lv _) = Just lv
assignLv _             = Nothing

count :: (Eq a, Foldable t) => t a -> [(a, Int)]
count = foldr impl []
    where
        impl a [] = [(a, 1)]
        impl a (x : xs) | fst x == a = (a, snd x + 1) : impl a xs
                        | otherwise  = impl a xs

killLvs :: IR -> [Expr]
killLvs ir =
        let assigns = fromJust <$> V.filter isJust (assignLv <$> stmts ir)
        in  V.toList assigns

prsv :: IR -> S.Set Expr
prsv ir = lvalues ir `S.difference` S.fromList (killLvs ir)

genLvs :: IR -> S.Set Expr
genLvs ir = S.fromList $ fst <$> filter ((== 1) . snd) (count $ killLvs ir)

data Expr = Uni Op Type Expr
          | Bin Op Type Expr Expr
          | Call Expr [Expr]
          | Phi Expr Expr -- 这里的Expr只可能为Var，同时该Expr仅出现于SSA形式中，不想换IR了
          | Addr Expr
          | Mem Expr
          | Fun String Int -- 函数，最后一个成员是id
          | Var String Type
          | Str String
          | I Int
          | F Double
          deriving (Show, Eq, Ord, Generic)

lvalue :: Expr -> Bool
lvalue Mem{} = True
lvalue Var{} = True
lvalue _     = False

lvalues :: IR -> S.Set Expr
lvalues ir = S.unions $ lvaluesByStmt <$> V.toList (stmts ir)
    where
        lvaluesByStmt (Assign lv rv) =
                lvaluesByExpr lv `S.union` lvaluesByExpr rv
        lvaluesByStmt (CJump cv then_ else_) = lvaluesByExpr cv
        lvaluesByStmt (Expression ex       ) = lvaluesByExpr ex
        lvaluesByStmt (Return     (Just e) ) = lvaluesByExpr e
        lvaluesByStmt _                      = S.empty

        lvaluesByExpr (Uni _ _ e) = lvaluesByExpr e
        lvaluesByExpr (Bin _ _ le re) =
                lvaluesByExpr le `S.union` lvaluesByExpr re
        lvaluesByExpr (Call fe pes) =
                S.unions $ lvaluesByExpr fe : (lvaluesByExpr <$> pes)
        lvaluesByExpr (  Addr e ) = lvaluesByExpr e
        lvaluesByExpr e@(Mem  me) = S.insert e $ lvaluesByExpr me
        lvaluesByExpr e@Var{}     = S.insert e S.empty
        lvaluesByExpr _           = S.empty

data Op = ADD
        | SUB
        | MUL
        | DIV
        | MOD
        | BIT_NO
        | BIT_AND
        | BIT_OR
        | BIT_XOR
        | BIT_LSHLFT
        | BIT_RSHLFT
        | EQ_
        | NEQ_
        | GT_
        | GTEQ
        | LT_
        | LTEQ
        | UMINUS
        | NOT
        | AND
        | OR
        | CAST
        deriving (Show, Eq, Ord, Generic)

transformOp :: String -> Op
transformOp "+"   = ADD
transformOp "-"   = SUB
transformOp "*"   = MUL
transformOp "/"   = DIV
transformOp "%"   = MOD
transformOp "~"   = BIT_NO
transformOp "&"   = BIT_AND
transformOp "|"   = BIT_OR
transformOp "^"   = BIT_XOR
transformOp "<<"  = BIT_LSHLFT
transformOp ">>"  = BIT_RSHLFT
transformOp "=="  = EQ_
transformOp "!="  = NEQ_
transformOp ">"   = GT_
transformOp ">="  = GTEQ
transformOp "<"   = LT_
transformOp "<="  = LTEQ
transformOp "!"   = NOT
transformOp "and" = AND
transformOp "or"  = OR


instance Out Stmt
instance Out Expr
instance Out Op
instance Out Type
