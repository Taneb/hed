{-# LANGUAGE GADTs #-}
module Hed.Command where

import Data.Proxy
import Text.Megaparsec

import Hed
import Hed.Address

class IsCommand a where
  runCommand :: [Int] -> Body -> a -> IO (Either String (Body, Maybe Int))
  parseCommand :: Parser SomeAddress -> Parser a

data SomeCommandType where
  SomeCommandType :: IsCommand a => Proxy a -> SomeCommandType

data SomeCommand where
  SomeCommand :: IsCommand a => a -> SomeCommand

parseSomeCommand :: Parser SomeAddress -> [SomeCommandType] -> Parser SomeCommand
parseSomeCommand addressParser = choice . fmap getParser
  where
    getParser (SomeCommandType p) = SomeCommand <$> fromProxy p

    fromProxy :: IsCommand a => Proxy a -> Parser a
    fromProxy _ = parseCommand addressParser
