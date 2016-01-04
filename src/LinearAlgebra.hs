module LinearAlgebra
  ( readMatrix
  ) where

--------------------------------------------------------------------------------
import           Data.Array.Accelerate
import           Text.Parsec
--------------------------------------------------------------------------------
import qualified Data.Array.Accelerate as A
--------------------------------------------------------------------------------
type Matrix e = Array DIM2 e
--------------------------------------------------------------------------------

readMatrix :: String -> Either ParseError (Matrix Int)
readMatrix = parse matrixP ""
  where
    matrixP = do
      rows <- dim
      endOfLine
      cols <- dim
      endOfLine
      elements <- lines
      return $ A.fromList (Z :. rows :. cols) (concat elements)
    dim    = toInt <$> many1 digit
    lines  = (toInt <$> many1 digit) `sepBy` tab `endBy` endOfLine
    toInt  = read :: String -> Int


add :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
add = A.zipWith (+)


mul :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
mul = undefined


nmf :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Matrix a)
nmf = A.map (^2)
