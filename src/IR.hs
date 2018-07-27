{-# LANGUAGE DeriveGeneric #-}

module IR where

import           GHC.Generics                   ( Generic )
import           Text.PrettyPrint.GenericPretty ( Out )


data IR = S Stmt
        | E Expr
        deriving (Show, Eq, Ord, Generic)

data Stmt = Assign Expr Expr
          | CJump Expr Stmt Stmt
          | Jump Stmt
          | Switch [(Expr, Stmt)] Stmt
          | Label String
          | ExprStmt Expr
          | Return Expr
          deriving (Show, Eq, Ord, Generic)

data Expr = Uni Op Expr
          | Bin Op Expr Expr
          | Call Expr [Expr]
          | Addr Expr
          | Mem Expr
          | Var String
          | Int Integer
          | Str String
          deriving (Show, Eq, Ord, Generic)

data Op = ADD
        | SUB
        | MUL
        | S_DIV
        | U_DIV
        | S_MOD
        | U_MOD
        | BIT_NO
        | BIT_AND
        | BIT_OR
        | BIT_XOR
        | BIT_LSHLFT
        | BIT_RSHLFT
        | ARITH_RSHIFT
        | EQ
        | NEQ
        | S_GT
        | S_GTEQ
        | S_LT
        | S_LTEQ
        | U_GT
        | U_GTEQ
        | U_LT
        | U_LTEQ
        | UMINUS
        | NOT
        | S_CAST
        | U_CAST
        deriving (Show, Eq, Ord, Generic)

