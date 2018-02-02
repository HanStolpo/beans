module Parser.AST where

import Control.Exception (Exception)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, intercalate, unpack)
import Data.Time.Calendar (Day)
import Text.Parsec (ParseError)

newtype ParseException =
  ParseException ParseError
  deriving (Show)

instance Exception ParseException

data Directive
  = Statement Day
              Statement
  | Config Config

data Statement
  = Price CommodityName
          Decimal
          CommodityName
  | Open AccountName
         [CommodityName]
  | Balance AccountName
            Decimal
            CommodityName
  | Transaction Flag
                Text
                [Tag]
                [Posting]
  | Close AccountName
  deriving (Eq, Ord, Show)

data Config
  = Option Text
           Text
  | Include FilePath
  deriving (Eq, Show, Ord)

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Show, Ord)

newtype Tag =
  Tag Text
  deriving (Show, Eq, Ord)

data Posting
  = Posting AccountName
            Decimal
            CommodityName
            (Maybe Lot)
  | Wildcard AccountName
  deriving (Show, Eq, Ord)

data Lot =
  Lot Decimal
      CommodityName
      (Maybe Day)
      (Maybe Text)
  deriving (Show, Eq, Ord)

newtype CommodityName =
  CommodityName Text
  deriving (Show, Eq, Ord)

newtype AccountName =
  AccountName [Text]
  deriving (Eq, Ord)

instance Show AccountName where
  show (AccountName n) = (unpack . intercalate ":") n
