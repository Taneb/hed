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

type Line = T.Text
type Body = V.Vector Line

type Parser = Parsec Void T.Text

class IsAddress a where
  runAddress :: Body -> Int -> a -> Maybe Int
  parseAddress :: Parser a

data SomeAddressType where
  SomeAddressType :: IsAddress a => Proxy a -> SomeAddressType

data SomeAddress where
  SomeAddress :: IsAddress a => a -> SomeAddress

runSomeAddress :: Body -> Int -> SomeAddress -> Maybe Int
runSomeAddress body curline (SomeAddress addr) = runAddress body curline addr

class IsCommand a where
  runCommand :: [Int] -> Body -> a -> IO (Either String (Body, Maybe Int))
  parseCommand :: Parser SomeAddress -> Parser SomeCommand -> Parser a

data SomeCommandType where
  SomeCommandType :: IsCommand a => Proxy a -> SomeCommandType

data SomeCommand where
  SomeCommand :: IsCommand a => a -> SomeCommand

newtype Literal = Literal Int

instance IsAddress Literal where
  runAddress body _ (Literal a) | a < fromIntegral (V.length body) = Just a
                                | otherwise = Nothing
  parseAddress = fmap (Literal . foldl (\n x -> digitToInt x + 10 * n) 0) (some digitChar)

data Print = Print

instance IsCommand Print where
  runCommand (to:from:_) body Print =
    let l = V.length body
    in if 1 <= from && from <= to && to <= l
      then do
        V.mapM_ T.putStrLn (V.unsafeSlice (from-1) (to-from) body)
        return (Right (body, Just to))
      else return (Left "Invalid address")
  parseCommand _ = Print <$ char 'p'

parseSomeAddress :: [SomeAddressType] -> Parser SomeAddress
parseSomeAddress = choice . fmap getParser
  where
    getParser :: SomeAddressType -> Parser SomeAddress
    getParser (SomeAddressType p) = SomeAddress <$> fromProxy p

    fromProxy :: IsAddress a => Proxy a -> Parser a
    fromProxy _ = parseAddress

parseSomeCommand :: Parser SomeAddress -> [SomeCommandType] -> Parser SomeCommand
parseSomeCommand addressParser = choice . fmap getParser
  where
    getParser :: SomeCommandType -> Parser SomeCommand
    getParser (SomeCommandType p) = SomeCommand <$> fromProxy p

    fromProxy :: IsCommand a => Proxy a -> Parser a
    fromProxy _ = parseCommand addressParser

data Instruction = Instruction {
  address :: [SomeAddress],
  command :: SomeCommand
  }




main :: IO ()
main = putStrLn "Hello, Haskell!"
