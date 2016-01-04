module Main
  where
--------------------------------------------------------------------------------
import Data.Array.Accelerate             as A
import Data.Array.Accelerate.CUDA        as I
import System.Environment (getArgs)
import LinearAlgebra
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  case args of
    [fromFile] -> do
      mmatrix <- readMatrix <$> readFile fromFile
      case mmatrix of
        Left err -> print err
        Right matrix -> do
          let t = I.run (transpose (use matrix))
          print . A.toList $ t

    _ -> putStrLn "Please provide an input file!"
