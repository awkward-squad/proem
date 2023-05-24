module Proem.Text
  ( fromInt,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Lazy.Builder
import Prelude

-- | Construct a text from an int.
fromInt :: Int -> Text
fromInt n =
  -- Benchmarking seems to indicate going through a proper builder becomes faster at 5 characters
  if n >= 0
    then if n < 9999 then stringy n else buildery n
    else if n >= -999 then stringy n else buildery n
  where
    stringy :: Int -> Text
    stringy =
      Text.pack . show

    buildery :: Int -> Text
    buildery =
      Text.Lazy.toStrict . Text.Lazy.Builder.toLazyText . Text.Lazy.Builder.decimal
