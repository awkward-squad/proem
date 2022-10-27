module Proem.Seq
  ( -- * Data.Foldable
    toList,
  )
where

import qualified Data.Foldable
import Data.Sequence (Seq)

toList :: Seq a -> [a]
toList =
  Data.Foldable.toList
{-# INLINE toList #-}
