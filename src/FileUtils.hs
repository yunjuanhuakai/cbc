module FileUtils
    ( readSource
    , runIO
    , findSource
    )
where

import           Data.List
import           Helper
import           IState
import           Control.Monad.State.Class
import           Control.Monad                  ( forM )
import           System.IO
import           System.Directory               ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )
import           Control.Monad.IO.Class
import           System.IO.Error
import           Control.Monad.Except

runIO :: IO a -> Cb a
runIO x = liftIO (tryIOError x) >>= either (throwError . Msg . show) return

findSource :: Cb [FilePath]
findSource = do
    i     <- get
    paths <- runIO $ getRecursiveContents $ projectDriectory i
    return $ filter (isSuffixOf ".cb") paths

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory then getRecursiveContents path else return [path]
    return (concat paths)

readSource :: FilePath -> Cb String
readSource f = do
    i <- get
    runIO $ readSource' (projectDriectory i </> f)

readSource' :: FilePath -> IO String
readSource' f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    hGetContents h
