{-# LANGUAGE DeriveGeneric #-}

module IState where

import qualified Ast                           as A
import qualified IR                            as R
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )
import           Text.PrettyPrint.GenericPretty ( Out )

data Scope = Scope
  { parent    :: Maybe Scope
  , decls     :: Map.Map String A.Declaration
  , children  :: [Scope]
  }
  deriving (Eq, Show, Generic)

data IState = IState
  {
-- ast
    types            :: Map.Map String A.Type
  , scope            :: Scope
  , projectDriectory :: FilePath
-- check
  , exprType :: Map.Map A.Expr A.Type
-- gen ir
  , level            :: Integer
  , labelCount       :: Integer
  , stmts            :: [R.Stmt]
  , breakStack       :: [R.Stmt]
  , continueStack    :: [R.Stmt]
  }
  deriving (Show, Eq, Generic)

cbInit :: IState
cbInit = IState typeInit
                scopeInit
                driectInit
                exprType
                levelInit
                labelCountInit
                stmtsInit
                brackStackInit
                continueStackInit
 where
  typeInit          = Map.empty
  scopeInit         = Scope Nothing Map.empty []
  driectInit        = ""
  exprType          = Map.empty
  levelInit         = 0
  labelCountInit    = 0
  stmtsInit         = []
  brackStackInit    = []
  continueStackInit = []

topScope :: Scope
topScope = Scope {parent = Nothing, decls = Map.fromList [], children = []}

searchByScope :: String -> IState -> Maybe A.Declaration
searchByScope name ist = impl $ scope ist
 where
  impl (Scope Nothing  vs _) = Map.lookup name vs
  impl (Scope (Just p) vs _) = case Map.lookup name vs of
    Just t  -> Just t
    Nothing -> impl p
