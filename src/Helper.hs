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

queryType :: Expr -> Cb Type
queryType e = fmap (\s -> exprTypes s Map.! e) get

queryTypeTwo :: Expr -> Expr -> Cb Type
queryTypeTwo le re = do
  lt <- fmap (\s -> exprTypes s Map.! le) get
  rt <- fmap (\s -> exprTypes s Map.! re) get
  pormotTwo lt rt

findHandlerById :: Int -> Cb DeclHandler
findHandlerById id = fmap (\ist -> handlers ist Map.! id) get

offset' :: Expr -> Cb Int
offset' (Member _ name expr) = do
  t <- queryType expr
  case offset t name of
    Just n -> pure n
    Nothing -> fail "错误的成员变量偏移量计算"
offset' _ = fail "无法计算偏移量"

tmpVar :: Expr -> Cb Expr
tmpVar e = do
  t <- queryType e
  ist <- get
  let id = declCount ist + 1
  let handler = DeclHandler t
  put $ ist 
    { declCount = id
    , handlers = Map.insert id handler $ handlers ist
    }
  pure $ Decl NoFC ("#tmp" ++ show id) id