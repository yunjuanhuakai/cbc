{-# LANGUAGE DeriveGeneric, ConstraintKinds, FlexibleContexts #-}

module Type.State where

import           Ast
import           IState
import           Helper
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )
import           Text.PrettyPrint.GenericPretty ( Out )
import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Control.Monad.Trans.State.Strict
                                                ( get
                                                , put
                                                , evalState
                                                , StateT(..)
                                                )

data Flag = Checking | Checked
  deriving (Eq, Show, Generic)

data CheckState = CheckState
  {
    rcursive :: Map.Map Type Flag
  , exprTypes ::  Map.Map Expr Type
  , ist :: IState
  }
  deriving (Eq, Show, Generic)

stateInit :: IState -> CheckState
stateInit ist = CheckState {rcursive = Map.empty, exprTypes = Map.empty, ist = ist }

type CbCheck = StateT CheckState (ExceptT Err Cb)