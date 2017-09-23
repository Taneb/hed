{-# LANGUAGE GADTs #-}
module Main where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Proxy
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Hed
import Hed.Address
import Hed.Address.Literal
import Hed.Command
import Hed.Command.Print

data Instruction = Instruction {
  address :: [SomeAddress],
  command :: SomeCommand
  }

main :: IO ()
main = putStrLn "Hello, Haskell!"
