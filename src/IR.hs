{-# LANGUAGE DeriveGeneric #-}

module IR where

import qualified Ast                           as A
import           GHC.Generics                   ( Generic )
import           Text.PrettyPrint.GenericPretty ( Out )

data IR = S Stmt
        | E Expr
        deriving (Show, Eq, Ord, Generic)

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

data Stmt = Assign Expr Expr
          | CJump Expr Stmt Stmt
          | Jump Stmt
          | Label String
          | Expression Expr
          | Return (Maybe Expr)
          deriving (Show, Eq, Ord, Generic)

data Expr = Uni Op Type Expr
          | Bin Op Type Expr Expr
          | Call Expr [Expr]
          | Addr Expr
          | Mem Expr
          | Var String Int
          | Str String
          | I Int
          | F Float
          deriving (Show, Eq, Ord, Generic)

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
        | CAST
        deriving (Show, Eq, Ord, Generic)

transformOp :: String -> Op
transformOp "+"  = ADD
transformOp "++" = ADD
transformOp "-"  = SUB
transformOp "--" = SUB
transformOp "*"  = MUL
transformOp "/"  = DIV
transformOp "%"  = MOD
transformOp "~"  = BIT_NO
transformOp "&"  = BIT_AND
transformOp "|"  = BIT_OR
transformOp "^"  = BIT_XOR
transformOp "<<" = BIT_LSHLFT
transformOp ">>" = BIT_RSHLFT
transformOp "==" = EQ_
transformOp "!=" = NEQ_
transformOp ">"  = GT_
transformOp ">=" = GTEQ
transformOp "<"  = LT_
transformOp "<=" = LTEQ
transformOp "!"  = NOT