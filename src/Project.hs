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
import qualified IR
import           Ast
import           IState
import qualified Type.Resolver                 as TC

runMain :: Cb a -> IO (a, IState)
runMain prog = do
  res <- runExceptT $ runStateT prog cbInit
  case res of
    Left  e -> fail $ show e
    Right r -> return r

cbMain :: FilePath -> Cb [Unit]
cbMain home = do
  ist  <- get
  path <- runIO $ canonicalizePath home
  put $ ist { projectDriectory = path }
  sourceNames <- findSource
  sources     <- mapM readSource sourceNames
  zipWithM parser' sourceNames sources

runparser :: Parser st res -> st -> String -> String -> Either ParseError res
runparser p i inputname s =
  case P.parse (runWriterT (evalStateT p i)) inputname s of
    Left  err -> Left $ ParseError s err
    Right v   -> Right $ fst v

parser' :: FilePath -> String -> Cb Unit
parser' fname input = do
  u   <- parserUnit fname input
  TC.unit u
  
parseImports :: FilePath -> String -> Cb (Maybe Mark, [Declaration])
parseImports fname input = do
  i <- get
  case runparser imports_ i fname input of
    Left  err            -> fail $ show err
    Right (mrk, ps_list) -> do
      decls <- parseImportDecls ps_list
      return (mrk, decls)

decls' :: Maybe Mark -> CbParser Declaration -> CbParser ([Declaration], IState)
decls' Nothing _ = (,) <$> pure [] <*> get
decls' (Just m) decl = restore m *> ((,) <$> P.many decl <*> get)

parserUnit :: FilePath -> String -> Cb Unit
parserUnit fname input = do
  (mrk, ds) <- parseImports fname input
  i <- get
  case runparser (decls' mrk declaration) i fname input of 
      Left  err    -> fail $ show err
      Right (x, i) -> do
        put i
        return $ Unit (ds ++ x)

parseImportDecls :: [Import] -> Cb [Declaration]
parseImportDecls []  = pure []
parseImportDecls ims = foldl1 (liftA2 (++)) parseImportDecls'
 where
  parseImportDecl fname = do
    file_in      <- readSource fname
    (mrk, ds) <- parseImports fname file_in
    i            <- get
    case runparser (decls' mrk declarationByImport) i fname file_in of
      Left  err    -> fail $ show err
      Right (x, i) -> do
        put i
        return (ds ++ x)
  parseImportDecls' = map (parseImportDecl . (++ ".cbh") . importPath) ims
