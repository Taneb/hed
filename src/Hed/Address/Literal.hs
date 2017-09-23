module Hed.Address.Literal where

import Data.Char
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

import Hed.Address

data Literal = Literal Int

instance IsAddress Literal where
  runAddress body _ (Literal a) | a < V.length body = Just a
                                | otherwise = Nothing

  parseAddress = Literal . foldl (\n x -> digitToInt x + 10 * n) 0 <$> some digitChar
