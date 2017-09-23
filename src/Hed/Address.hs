{-# LANGUAGE GADTs #-}
module Hed.Address where

import Data.Proxy
import Text.Megaparsec

import Hed

class IsAddress a where
  runAddress :: Body -> Int -> a -> Maybe Int
  parseAddress :: Parser a

data SomeAddressType where
  SomeAddressType :: IsAddress a => Proxy a -> SomeAddressType

data SomeAddress where
  SomeAddress :: IsAddress a => a -> SomeAddress

runSomeAddress :: Body -> Int -> SomeAddress -> Maybe Int
runSomeAddress body curline (SomeAddress addr) = runAddress body curline addr

parseSomeAddress :: [SomeAddressType] -> Parser SomeAddress
parseSomeAddress = choice . fmap getParser
  where
    getParser (SomeAddressType p) = SomeAddress <$> fromProxy p

    fromProxy :: IsAddress a => Proxy a -> Parser a
    fromProxy _ = parseAddress
