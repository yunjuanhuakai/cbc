{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Helper where

import qualified Text.Megaparsec               as P
import           Control.Monad.State.Strict     ( StateT(..) )
import           Control.Monad.Writer.Strict    ( WriterT(..) )
import           Data.Void
import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Ast
import           IState

data Err = Msg String
    deriving Show

type Parser st = StateT st (WriterT FC (P.Parsec Void String))
type CbParser = Parser IState
type Cb = StateT IState (ExceptT Err IO)
type Mark = P.State String
data ParseError = ParseError String (P.ParseError (P.Token String) Void)
    deriving Show
