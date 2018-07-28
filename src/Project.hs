module Project where

import qualified Text.Megaparsec               as P
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( forM
                                                , zipWithM
                                                )
import           Control.Monad.State.Strict     ( evalStateT
                                                , get
                                                , put
                                                )
import           Control.Monad.Writer.Strict    ( runWriterT )
import           System.Directory

import           Helper
import           FileUtils
import           Parser
import           Control.Monad.Trans.Except     ( runExceptT )
import           Control.Monad.Trans.State.Strict
                                                ( runStateT )
import           Ast
import           IState

runMain :: Cb a -> IO a
runMain prog = do
  res <- runExceptT $ runStateT prog cbInit
  case res of
    Left  e -> fail $ show e
    Right r -> return $ fst r

cbMain :: FilePath -> Cb [Unit]
cbMain home = do
  ist  <- get
  path <- runIO $ canonicalizePath home
  put $ ist { projectDriectory = path }
  sourceNames <- findSource
  sources     <- mapM readSource sourceNames
  zipWithM parserUnit sourceNames sources

runparser :: Parser st res -> st -> String -> String -> Either ParseError res
runparser p i inputname s =
  case P.parse (runWriterT (evalStateT p i)) inputname s of
    Left  err -> Left $ ParseError s err
    Right v   -> Right $ fst v

parserUnit :: FilePath -> String -> Cb Unit
parserUnit fname input = do
  i <- get
  case runparser unit i fname input of
    Left  err -> fail $ show err
    Right u   -> do
      decls <- parseImportDecls $ imports u
      return $ u { declarations = decls ++ declarations u }

parseImportDecls :: [Import] -> Cb [Declaration]
parseImportDecls []  = pure []
parseImportDecls ims = foldl1 (liftA2 (++)) parseImportDecls'
 where
  parseImportDecl fname = do
    file_in <- readSource fname
    mrk     <- parseImports fname file_in
    i       <- get
    case runparser (importDecl mrk) i fname file_in of
      Left  err    -> fail $ show err
      Right (x, i) -> do
        put i
        return x
  importDecl Nothing = do
    i <- get
    return ([], i)
  importDecl (Just m) = do
    restore m
    i  <- get
    ds <- P.many declarationByImport
    return (ds, i)
  parseImportDecls' = map (parseImportDecl . (++ ".cbh") . importPath) ims

parseImports :: FilePath -> String -> Cb (Maybe Mark)
parseImports fname input = do
  i <- get
  case runparser imports_ i fname input of
    Left  err            -> fail $ show err
    Right (mrk, ps_list) -> do
      parseImportDecls ps_list
      return mrk
