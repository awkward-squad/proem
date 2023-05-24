module Main where

import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int qualified as Text.Lazy.Builder
import ParkBench
import Proem.Text qualified

main :: IO ()
main = do
  benchmark
    [ function "pack . show" (Text.pack . show) (-1000),
      function "toStrict . toLazyText . decimal" (Text.Lazy.toStrict . Text.Lazy.Builder.toLazyText . Text.Lazy.Builder.decimal) (-1000),
      function "Proem.Text.fromInt" Proem.Text.fromInt (-1000)
    ]
