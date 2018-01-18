module Data.Holdings
  ( Holdings(..)
  , toList
  , fromList
  , filter
  ) where

import Data.Amount (Amount(..))
import Data.Commodity (CommodityName)
import qualified Data.Map.Lazy as M
import Data.Text.Prettyprint.Doc (Pretty, (<+>), pretty, vsep)
import Prelude hiding (filter)

newtype Holdings a = Holdings
  { _unHoldings :: M.Map CommodityName a
  } deriving (Show, Eq, Functor)

instance (Show a) => Pretty (Holdings a) where
  pretty (Holdings h) = vsep (map f (M.toList h))
    where
      f (k, v) = pretty k <+> (pretty . show) v

instance Num a => Monoid (Holdings a) where
  mempty = Holdings M.empty
  (Holdings a) `mappend` (Holdings b) = Holdings (M.unionWith (+) a b)

toList :: Holdings a -> [Amount a]
toList h = (M.toList . _unHoldings) h >>= \(c, a) -> return $ Amount a c

fromList :: Num a => [Amount a] -> Holdings a
fromList = Holdings . M.fromListWith (+) . fmap (\(Amount a c) -> (c, a))

filter :: (a -> Bool) -> Holdings a -> Holdings a
filter f h = Holdings $ M.filter f (_unHoldings h)