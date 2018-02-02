module Parser
  ( parse
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Decimal (Decimal)
import Data.Functor.Identity (Identity)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
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

space :: Parser Char
space = char ' '

dash :: Parser Char
dash = char '-'

colon :: Parser Char
colon = char ':'

hash :: Parser Char
hash = char '#'

doubleQuote :: Parser Char
doubleQuote = char '\"'

token :: Parser a -> Parser a
token p = p <* many space

readInt :: (Read a) => Int -> Parser a
readInt n = read <$> count n digit

date :: Parser Day
date =
  token $
  fromGregorian <$> readInt 4 <* dash <*> readInt 2 <* dash <*> readInt 2

text :: Parser Char -> Parser Text
text p = pack <$> many p

surroundedBy :: Parser a -> Parser b -> Parser a
surroundedBy p s = between s s p

symbol :: String -> Parser Text
symbol i = token $ pack <$> string i

quotedString :: Parser Text
quotedString = token $ text (noneOf "\"") `surroundedBy` doubleQuote

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

decimal :: Parser Decimal
decimal = token $ sign <*> fractional2 True

accountNameSegment :: Parser Text
accountNameSegment = cons <$> letter <*> text alphaNum

accountName :: Parser AccountName
accountName = token $ AccountName <$> accountNameSegment `sepBy` colon

commodityName :: Parser CommodityName
commodityName = token $ CommodityName . pack <$> many1 alphaNum

postingPrice :: Parser ()
postingPrice =
  (try (symbol "@@") <|> symbol "@") >> decimal >> commodityName >> pure ()

lot :: Parser Lot
lot = braces $ Lot <$> decimal <*> commodityName <*> date' <*> label'
  where
    date' = optionMaybe (symbol "," >> date)
    label' = optionMaybe (symbol "," >> quotedString)

posting :: Parser Posting
posting = newline >> symbol " " >> (try posting' <|> wildcard)
  where
    posting' =
      Posting <$> accountName <*> decimal <*> commodityName <*> optionMaybe lot <*
      optionMaybe postingPrice
    wildcard = Wildcard <$> accountName

flagIncomplete :: Parser Flag
flagIncomplete = Incomplete <$ symbol "!"

flagComplete :: Parser Flag
flagComplete = Complete <$ symbol "*"

flag :: Parser Flag
flag = flagComplete <|> flagIncomplete

tag :: Parser Tag
tag = Tag <$> (cons <$> hash <*> text alphaNum)

statement :: Parser Statement
statement = open <|> close <|> balance <|> price <|> transaction
  where
    transaction =
      Transaction <$> flag <*> quotedString <*> many tag <*> many1 (try posting)
    open =
      Open <$ symbol "open" <*> accountName <*> commodityName `sepBy` symbol ","
    close = Close <$ symbol "close" <*> accountName
    balance =
      Balance <$ symbol "balance" <*> accountName <*> decimal <*> commodityName
    price =
      Price <$ symbol "price" <*> commodityName <*> decimal <*> commodityName

config :: Parser Config
config = include <|> option
  where
    include = symbol "include" >> Include . unpack <$> quotedString
    option = symbol "option" >> Option <$> quotedString <*> quotedString

directive :: Parser Directive
directive = (Statement <$> date <*> statement) <|> Config <$> config

eol :: Parser ()
eol = void $ token newline

comment :: Parser ()
comment = void (oneOf ";#" >> anyChar `manyTill` try eol)

block :: Parser a -> Parser a
block p = p `surroundedBy` many (comment <|> eol)

directives :: Parser [Directive]
directives = many (block directive) <* eof

parse :: (MonadThrow m) => FilePath -> Text -> m [Directive]
parse f t =
  case P.parse directives f t of
    Left e -> throwM $ ParseException e
    Right d -> return d
