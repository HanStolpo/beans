{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parser.Pretty where

import Data.Decimal (Decimal)
import Data.Text.Prettyprint.Doc
       (Doc, Pretty, (<+>), (<>), cat, dquotes, encloseSep, hardline,
        indent, line, pretty, sep, vcat, vsep)
import Data.Time.Calendar (Day)

import Parser.AST
       (AccountName(..), CommodityName(..), Config(..), Directive(..),
        Flag(..), Lot(..), Posting(..), Statement(..), Tag(..))

prettyPrint :: [Directive] -> Doc a
prettyPrint = vsep . map ((<> hardline) . pretty)

instance Pretty Directive where
  pretty (Statement t d) = pretty t <+> pretty d
  pretty (Config t) = pretty t

instance Pretty Statement where
  pretty (Transaction flag desc tags postings) =
    pretty flag <+>
    dquotes (pretty desc) <+>
    cat (map pretty tags) <> line <> (indent 2 . vcat) (map pretty postings)
  pretty (Balance account amount commodity) =
    "balance" <+> pretty account <+> pretty amount <+> pretty commodity
  pretty (Open account commodities) =
    "open" <+> pretty account <+> sep (map pretty commodities)
  pretty (Close account) = "close" <+> pretty account
  pretty (Price source amount target) =
    "price" <+> pretty source <+> pretty amount <+> pretty target

instance Pretty Config where
  pretty (Include filePath) = "include" <+> pretty filePath
  pretty (Option d t) = "option" <+> pretty d <+> pretty t

instance Pretty Decimal where
  pretty = pretty . show

instance Pretty AccountName where
  pretty = pretty . show

instance Pretty CommodityName where
  pretty (CommodityName n) = pretty n

instance Pretty Day where
  pretty = pretty . show

instance Pretty Flag where
  pretty Complete = "*"
  pretty Incomplete = "!"

instance Pretty Tag where
  pretty (Tag t) = pretty t

instance Pretty Lot where
  pretty (Lot amount commodity date label) =
    encloseSep "{" "}" "," $
    [pretty amount <+> pretty commodity, pretty date] ++
    case label of
      Nothing -> []
      _ -> [pretty label]

instance Pretty Posting where
  pretty (Posting account amount commodity lot) =
    pretty account <+> pretty amount <+> pretty commodity <+> pretty lot
  pretty (Wildcard account) = pretty account
