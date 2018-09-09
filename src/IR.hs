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
  show (IR name rt params stmts) = "fun " ++  name ++ "(" ++ paramsStr ++  "): " ++ show rt ++ " {\n" ++ stmtsStr ++ "\n}"
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
          | Ptr
          | Struct Int
          deriving (Show, Eq, Ord, Generic)

transformType :: A.Type -> Type
transformType (A.CbConst t)  = transformType t
transformType A.CbPtr{}      = Ptr
transformType A.CbArray{}    = Ptr
transformType A.CbFunction{} = Ptr
transformType u@A.CbUnion{}  = Struct $ A.sizeof u
transformType s@A.CbStruct{} = Struct $ A.sizeof s
transformType A.CbLong       = I64
transformType A.CbULong      = UI64
transformType A.CbInt        = I32
transformType A.CbUInt       = UI32
transformType A.CbFloat      = F32
transformType A.CbDouble     = F64
transformType A.CbChar       = I8
transformType A.CbUChar      = UI8
transformType A.CbBool       = I8
transformType A.CbVoid       = I8

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

data Expr = Uni { op :: String, ty :: Type, var :: Expr }
          | Bin { op :: String, ty :: Type, lv :: Expr, rv :: Expr}
          | Call { fun :: Expr, ps :: [Expr] }
          | Phi { lv :: Expr, rv :: Expr } -- 这里的Expr只可能为Var，同时该Expr仅出现于SSA形式中，不想换IR了
          | Addr { var :: Expr }
          | Mem { var :: Expr, dom :: Expr }
          | Fun String Int -- 函数，最后一个成员是id
          | Var String Type
          | Str String
          | I Int
          | F Double
          deriving (Eq, Ord, Generic)

isPtr :: Expr -> Bool
isPtr (Var _ Ptr) = True
isPtr _           = False

isStruct :: Expr -> Bool
isStruct (Var _ Struct{}) = True
isStruct _ = False

isAddr :: Expr -> Bool
isAddr (Addr _) = True
isAddr _        = False

isMem :: Expr -> Bool
isMem (Mem _ _) = True
isMeme = False

isBin :: Expr -> Bool
isBin Bin{} = True
isBin _ = False

instance Show Expr where
  show (Uni op t e) = op ++ " " ++ show e
  show (Bin op t l r) = show l ++ " " ++ op ++ " " ++ show r
  show (Call fun params) = show fun ++ "(" ++ intercalate ", " (fmap show params) ++ ")"
  show (Phi l r) = "∅(" ++ show l ++ ", " ++ show r ++ ")"
  show (Addr e)  = "&(" ++ show e ++ ")"
  show (Mem e dom) = "*(" ++ show e ++ ", " ++ show dom ++ ")"
  show (Fun name id) = name
  show (Var name t) = name ++ ": " ++ show t
  show (Str s) = "\"" ++ s ++ "\""
  show (I i) = show i
  show (F f) = show f

lvalue :: Expr -> Bool
lvalue Mem{} = True
lvalue Var{} = True
lvalue _     = False

lvalues' :: [Stmt] -> S.Set Expr
lvalues' stmts = S.unions $ lvaluesByStmt <$> stmts
 where
  lvaluesByStmt (Assign lv rv) = lvaluesByExpr lv `S.union` lvaluesByExpr rv
  lvaluesByStmt (CJump cv then_ else_) = lvaluesByExpr cv
  lvaluesByStmt (Expression ex       ) = lvaluesByExpr ex
  lvaluesByStmt (Return     (Just e) ) = lvaluesByExpr e
  lvaluesByStmt _                      = S.empty

  lvaluesByExpr (Uni _ _ e    ) = lvaluesByExpr e
  lvaluesByExpr (Bin _ _ le re) = lvaluesByExpr le `S.union` lvaluesByExpr re
  lvaluesByExpr (Call fe pes) =
    S.unions $ lvaluesByExpr fe : (lvaluesByExpr <$> pes)
  lvaluesByExpr (Addr e) = lvaluesByExpr e
  lvaluesByExpr e@(Mem me dom) =
    S.insert e (lvaluesByExpr me `S.union` lvaluesByExpr dom)
  lvaluesByExpr e@Var{} = S.insert e S.empty
  lvaluesByExpr _       = S.empty

lvalues :: IR -> S.Set Expr
lvalues ir = lvalues' $ V.toList (stmts ir)

instance Out Stmt
instance Out Expr
instance Out Type