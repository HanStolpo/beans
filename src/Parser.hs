module Parser
  ( parse
  , recursiveParse
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Data.Decimal (Decimal)
import Data.Functor.Identity (Identity)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Text.Lazy.IO (readFile)
import Data.Time.Calendar (Day, fromGregorian)
import Prelude hiding (readFile)
import System.FilePath.Posix ((</>), takeDirectory)
import Text.Parsec
       ((<|>), alphaNum, anyChar, between, char, count, digit, eof,
        letter, many, many1, manyTill, newline, noneOf, oneOf, optionMaybe,
        sepBy, string, try)
import qualified Text.Parsec as P
import Text.Parsec.Number (fractional2, sign)

import Parser.AST
       (AccountName(..), CommodityName(..), Config(..), Directive(..),
        Flag(..), Lot(..), ParseException(..), Posting(..), Statement(..),
        Tag(..))

type Parser = P.ParsecT Text () Identity

token :: Parser a -> Parser a
token p = p <* many (char ' ')

date :: Parser Day
date =
  token $
  fromGregorian <$> readInt 4 <* dash <*> readInt 2 <* dash <*> readInt 2
  where
    readInt n = read <$> count n digit
    dash = char '-'

text :: Parser Char -> Parser Text
text p = pack <$> many p

surroundedBy :: Parser a -> Parser b -> Parser a
surroundedBy p s = between s s p

symbol :: String -> Parser Text
symbol i = token $ pack <$> string i

quotedString :: Parser Text
quotedString = token $ text (noneOf "\"") `surroundedBy` char '\"'

decimal :: Parser Decimal
decimal = token $ sign <*> fractional2 True

accountNameSegment :: Parser Text
accountNameSegment = cons <$> letter <*> text alphaNum

account :: Parser AccountName
account = token $ AccountName <$> accountNameSegment `sepBy` char ':'

commodity :: Parser CommodityName
commodity = token $ CommodityName . pack <$> many1 alphaNum

lot :: Parser Lot
lot = braces $ Lot <$> decimal <*> commodity <*> date' <*> label'
  where
    date' = optionMaybe (symbol "," >> date)
    label' = optionMaybe (symbol "," >> quotedString)
    braces = between (symbol "{") (symbol "}")

posting :: Parser Posting
posting = newline >> symbol " " >> (try posting' <|> wildcard)
  where
    posting' =
      Posting <$> account <*> decimal <*> commodity <*> optionMaybe lot <*
      optionMaybe postingPrice
    wildcard = Wildcard <$> account
    postingPrice =
      (try (symbol "@@") <|> symbol "@") >> decimal >> commodity >> pure ()

flag :: Parser Flag
flag = flagComplete <|> flagIncomplete
  where
    flagIncomplete = Incomplete <$ symbol "!"
    flagComplete = Complete <$ symbol "*"

tag :: Parser Tag
tag = token $ Tag <$> (cons <$> char '#' <*> text alphaNum)

statement :: Parser Statement
statement = open <|> close <|> balance <|> price <|> transaction
  where
    open = Open <$ symbol "open" <*> account <*> commodity `sepBy` symbol ","
    close = Close <$ symbol "close" <*> account
    balance = Balance <$ symbol "balance" <*> account <*> decimal <*> commodity
    price = Price <$ symbol "price" <*> commodity <*> decimal <*> commodity
    transaction = Transaction <$> flag <*> desc <*> tags <*> postings
      where
        tags = many tag
        desc = quotedString
        postings = many1 $ try posting

config :: Parser Config
config = include <|> option
  where
    include = symbol "include" >> Include . unpack <$> quotedString
    option = symbol "option" >> Option <$> quotedString <*> quotedString

directive :: Parser Directive
directive = stmt <|> cfg
  where
    stmt = Stmt <$> date <*> statement
    cfg = Cfg <$> config

space :: Parser ()
space = void $ many (comment <|> eol)
  where
    eol = void $ token newline
    comment = void $ oneOf ";#" >> anyChar `manyTill` try eol

directives :: Parser [Directive]
directives = many (directive `surroundedBy` space) <* eof

parse :: (MonadThrow m) => FilePath -> Text -> m [Directive]
parse f t =
  case P.parse directives f t of
    Left e -> throwM $ ParseException e
    Right d -> return d

recursiveParse :: (MonadIO m, MonadThrow m) => FilePath -> m [Directive]
recursiveParse f = do
  content <- liftIO $ readFile f
  ds <- parse f content
  let absPaths = [takeDirectory f </> r | r <- collectRelativePaths ds]
  (ds ++) . concat <$> traverse recursiveParse absPaths

collectRelativePaths :: [Directive] -> [FilePath]
collectRelativePaths ds = [relPath | (Cfg (Include relPath)) <- ds]
