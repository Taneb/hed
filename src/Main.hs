{-# LANGUAGE GADTs #-}
module Main where

import Hed.Address
import Hed.Command

data Instruction = Instruction {
  address :: [SomeAddress],
  command :: SomeCommand
  }

main :: IO ()
main = putStrLn "Hello, Haskell!"
