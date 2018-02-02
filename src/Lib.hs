module Lib
  ( doParse
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Data.Text.Lazy.IO (readFile)
import Data.Text.Prettyprint.Doc
import Parser (parse)
import Parser.AST (Config(..), Directive(..))
import Parser.Pretty ()
import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeDirectory)

collectRelativePaths :: [Directive] -> [FilePath]
collectRelativePaths directives =
  [relPath | (Config (Include relPath)) <- directives]

recursiveParse :: (MonadIO m, MonadThrow m) => FilePath -> m [Directive]
recursiveParse filePath = do
  fileContent <- liftIO $ readFile filePath
  directives <- parse filePath fileContent
  let dirPath = takeDirectory filePath
      absPaths = (dirPath </>) <$> collectRelativePaths directives
  (directives ++) . concat <$> traverse recursiveParse absPaths

prettyPrint :: [Directive] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

doParse :: (MonadIO m, MonadThrow m) => m ()
doParse = do
  (file:_) <- liftIO getArgs
  liftIO $ prettyPrint =<< recursiveParse file
