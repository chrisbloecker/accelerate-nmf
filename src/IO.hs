{-# LANGUAGE RecordWildCards #-}

module IO
  ( TSV (..)
  , readTSV
  , dim
  ) where

--------------------------------------------------------------------------------
import Text.Parsec
--------------------------------------------------------------------------------

data TSV a = TSV { header :: ![String]
                 , values :: ![[a]]
                 }
  deriving (Show)

--------------------------------------------------------------------------------

dim :: TSV a -> (Int, Int)
dim TSV {..} = (length values, length . head $ values)

readTSV :: String -> Either ParseError (TSV Int)
readTSV = parse matrix ""
  where
    matrix = do
             h <- header
             endOfLine
             m <- lines
             return $ TSV h m
    header = many (noneOf "\t\n") `sepBy` tab
    lines  = (toInt <$> many1 digit) `sepBy` tab `endBy` endOfLine
    toInt  = read :: String -> Int
