module Hed.Command.Print where

import qualified Data.Vector as V
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Hed.Command

data Print = Print

instance IsCommand Print where
  runCommand (to:from:_) body Print =
    if 1 <= from && from <= to && to <= V.length body
      then do
        V.mapM_ T.putStrLn (V.unsafeSlice (from-1) (to-from) body)
        return (Right (body, Just to))
      else return (Left "Invalid address")

  parseCommand _ = Print <$ char 'p'
