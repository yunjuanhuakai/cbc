{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Helper where

import qualified Text.Megaparsec               as P
import           Control.Monad.State.Strict     ( StateT(..), get, put )
import           Control.Monad.Writer.Strict    ( WriterT(..) )

import qualified Data.Map.Strict               as Map
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

pormot' :: Type -> Type -> Cb Type
pormot' source target =
  if pormot source target then pure target else fail "类型不匹配"

maybeToEither :: Maybe a -> Cb a
maybeToEither (Just a) = pure a
maybeToEither Nothing  = fail "类型不匹配"

pormotTwo :: Type -> Type -> Cb Type
pormotTwo t1 t2 = maybeToEither $ typeTable !! typeIndex t1 !! typeIndex t2

findHandlerById :: Int -> Cb DeclHandler
findHandlerById id = fmap (\ist -> handlers ist Map.! id) get
