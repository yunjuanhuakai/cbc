module Type where

import qualified Data.Map.Strict               as Map
import           Control.Monad.State.Strict     ( get
                                                , put
                                                )

import           Ast
import           Helper
import           IState

---------------------------------- resolver ------------------------------------------

typeResolver :: Unit -> Cb Unit
typeResolver (Unit imps decls) = Unit imps <$> mapM typeResolverOfDecl decls

paramResolver :: Param -> Cb Param
paramResolver (Param t name init) =
    Param <$> resolver t <*> pure name <*> mapM typeResolverOfExpr init

typeResolverOfDecl :: Declaration -> Cb Declaration
typeResolverOfDecl (Variable fc t name init) =
    Variable fc <$> resolver t <*> pure name <*> mapM typeResolverOfExpr init
typeResolverOfDecl (Function fc rt name params body) =
    Function fc
        <$> resolver rt
        <*> pure name
        <*> mapM paramResolver params
        <*> typeResolverOfStmt body
typeResolverOfDecl (UndefineFunction fc rt name params) =
    UndefineFunction fc
        <$> resolver rt
        <*> pure name
        <*> mapM paramResolver params
typeResolverOfDecl (Struct fc name params) =
    Struct fc name <$> mapM paramResolver params
typeResolverOfDecl (Union fc name params) =
    Union fc name <$> mapM paramResolver params
typeResolverOfDecl (Typedef fc t name) =
    Typedef fc <$> resolver t <*> pure name

typeResolverOfStmt :: Stmt -> Cb Stmt
typeResolverOfStmt (If fc expr then_ else_) =
    If fc
        <$> typeResolverOfExpr expr
        <*> typeResolverOfStmt then_
        <*> mapM typeResolverOfStmt else_
typeResolverOfStmt (Switch fc expr stmts) =
    Switch fc <$> typeResolverOfExpr expr <*> mapM typeResolverOfStmt stmts
typeResolverOfStmt (Case fc expr body) =
    Case fc <$> typeResolverOfExpr expr <*> typeResolverOfStmt body
typeResolverOfStmt (For fc init cond next body) =
    For fc
        <$> typeResolverOfStmt init
        <*> mapM typeResolverOfExpr cond
        <*> typeResolverOfStmt next
        <*> typeResolverOfStmt body
typeResolverOfStmt (While fc cond body) =
    While fc <$> typeResolverOfExpr cond <*> typeResolverOfStmt body
typeResolverOfStmt (DoWhile fc body cond) =
    DoWhile fc <$> typeResolverOfStmt body <*> typeResolverOfExpr cond
typeResolverOfStmt (Block fc decls stmts) =
    Block fc <$> mapM typeResolverOfDecl decls <*> mapM typeResolverOfStmt stmts
typeResolverOfStmt (Return fc expr) =
    Return fc <$> mapM typeResolverOfExpr expr
typeResolverOfStmt (Expression fc expr) =
    Expression fc <$> typeResolverOfExpr expr
typeResolverOfStmt s = pure s

typeResolverOfExpr :: Expr -> Cb Expr
typeResolverOfExpr (Funcall fc params fun) =
    Funcall fc <$> mapM typeResolverOfExpr params <*> typeResolverOfExpr fun
typeResolverOfExpr (SizeofType fc t) = SizeofType fc <$> resolver t
typeResolverOfExpr (SizeofExpr fc e) = SizeofExpr fc <$> typeResolverOfExpr e
typeResolverOfExpr (Assign fc lv rv) =
    Assign fc <$> typeResolverOfExpr lv <*> typeResolverOfExpr rv
typeResolverOfExpr (OpAssign fc s lv rv) =
    OpAssign fc s <$> typeResolverOfExpr lv <*> typeResolverOfExpr rv
typeResolverOfExpr (Binary fc op l r) =
    Binary fc op <$> typeResolverOfExpr l <*> typeResolverOfExpr r
typeResolverOfExpr (Unary fc op expr) = Unary fc op <$> typeResolverOfExpr expr
typeResolverOfExpr (Prefix fc op expr) =
    Prefix fc op <$> typeResolverOfExpr expr
typeResolverOfExpr (Suffix fc op expr) =
    Suffix fc op <$> typeResolverOfExpr expr
typeResolverOfExpr (Cast fc t e) = Cast fc <$> resolver t <*> pure e
typeResolverOfExpr (Cond fc cond then_ else_) =
    Cond fc
        <$> typeResolverOfExpr cond
        <*> typeResolverOfExpr then_
        <*> typeResolverOfExpr else_
typeResolverOfExpr (Address fc expr) = Address fc <$> typeResolverOfExpr expr
typeResolverOfExpr (Dereference fc expr) =
    Dereference fc <$> typeResolverOfExpr expr
typeResolverOfExpr (Member fc name expr) =
    Member fc name <$> typeResolverOfExpr expr
typeResolverOfExpr (PtrMember fc name expr) =
    PtrMember fc name <$> typeResolverOfExpr expr
typeResolverOfExpr (Arrayref fc ve ie) =
    Arrayref fc <$> typeResolverOfExpr ve <*> typeResolverOfExpr ie
typeResolverOfExpr e = pure e

resolver :: Type -> Cb Type
resolver (CbUnknown name) = do
    ist <- get
    case Map.lookup name (types ist) of
        Just t  -> pure t
        Nothing -> fail $ "未知的类型名" ++ name
resolver t = pure t
