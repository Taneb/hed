module Hed where

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void

import Text.Megaparsec

type Line = T.Text
type Body = V.Vector Line

type Parser = Parsec Void T.Text
