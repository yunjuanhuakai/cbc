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
  }
  deriving (Eq, Show, Generic)

data Flag = Checking | Checked
  deriving (Eq, Show, Generic)

data IState = IState
  {
-- ast
    types            :: Map.Map String A.Type
  , scope            :: Scope
  , projectDriectory :: FilePath
-- check
  , rcursive         :: Map.Map A.Type Flag
  , exprTypes        :: Map.Map A.Expr A.Type
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
                rcursiveInit
                exprTypesInit
                levelInit
                labelCountInit
                stmtsInit
                brackStackInit
                continueStackInit
 where
  typeInit          = Map.empty
  scopeInit         = Scope Nothing Map.empty
  driectInit        = ""
  levelInit         = 0
  rcursiveInit      = Map.empty
  exprTypesInit     = Map.empty
  labelCountInit    = 0
  stmtsInit         = []
  brackStackInit    = []
  continueStackInit = []

topScope :: Scope
topScope = Scope {parent = Nothing, decls = Map.fromList []}

searchByScope :: String -> IState -> Maybe A.Declaration
searchByScope name ist = impl $ scope ist
 where
  impl (Scope Nothing  vs) = Map.lookup name vs
  impl (Scope (Just p) vs) = case Map.lookup name vs of
    Just t  -> Just t
    Nothing -> impl p
