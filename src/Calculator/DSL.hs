module Calculator.DSL
  ( Statements
  , fromDirectives
  , State(..)
  , update
  ) where

import Control.Applicative ((<|>))
import Data.List as L
import qualified Data.Map.Lazy as M
import Data.Time.Calendar (Day)

import Parser.AST

type Statements = M.Map Day [Statement]

-- convert an unordered list of directives to a list of timesteps, ordered by
-- date
fromDirectives :: [Directive] -> Statements
fromDirectives = foldr insertDirective mempty

insertDirective :: Directive -> Statements -> Statements
insertDirective (Stmt d s) = M.alter upsert d
  where
    upsert f = fmap (L.insert s) (f <|> pure [])
insertDirective _ = id

data State = State
  { _date :: Day
  , _holdings :: ()
  , _prices :: ()
  , _transactions :: ()
  }

update :: Statements -> State -> State
update = undefined
