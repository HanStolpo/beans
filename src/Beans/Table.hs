module Beans.Table where

import           Data.List                (intercalate, transpose)
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)

-- a type for fill functions
type Filler = Int -> String -> String

-- a type for describing table columns
data ColDesc t = ColDesc
  { colTitleFill :: Filler
  , colTitle     :: String
  , colValueFill :: Filler
  , colValue     :: t -> String
  }

-- Table formatting inspired by:
-- https://stackoverflow.com/questions/5929377/format-list-output-in-haskell

fillLeft, fillRight, fillCenter :: a -> Int -> [a] -> [a]

fillLeft c n s = s ++ replicate (n - length s) c
fillRight c n s = replicate (n - length s) c ++ s
fillCenter c n s = replicate l c ++ s ++ replicate r c
    where x = n - length s
          l = x `div` 2
          r = x - l

left, right, center :: Int -> String -> String

left = fillLeft ' '
right = fillRight ' '
center = fillCenter ' '

showTable :: [ColDesc item] -> [item] -> [item] -> String
showTable coldefs items totals =
  let header = colTitle <$> coldefs
      rows = [[colValue coldef item | coldef <- coldefs] | item <- items]
      totalRows =
        [[colValue coldef footer | coldef <- coldefs] | footer <- totals]
      widths =
        [ maximum $ length <$> column
        | column <- transpose $ header : rows ++ totalRows
        ]
      separator = intercalate "-+-" [replicate width '-' | width <- widths]
      fillColumns fill columns =
        intercalate
          " | "
          [ fill c width column
          | (c, width, column) <- zip3 coldefs widths columns
          ]
   in unlines $
      fillColumns colTitleFill header :
      separator :
      (fillColumns colValueFill <$> rows) ++
      pure separator ++
      (fillColumns colValueFill <$> totalRows) ++ pure separator

formatStandard :: Scientific -> String
formatStandard = formatScientific Fixed (Just 2)