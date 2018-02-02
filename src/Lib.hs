module Lib
  ( doParse
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Parser (recursiveParse)
import Parser.Pretty (prettyPrint)
import System.Environment (getArgs)

doParse :: (MonadIO m, MonadThrow m) => m ()
doParse = do
  (file:_) <- liftIO getArgs
  liftIO $ prettyPrint <$> recursiveParse file >>= print
