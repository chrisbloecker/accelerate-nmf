{-# LANGUAGE RecordWildCards #-}
module Main
  where
--------------------------------------------------------------------------------
import Data.Array.Accelerate             as A
--import Data.Array.Accelerate.CUDA        as I
import Data.Array.Accelerate.Interpreter as I
import System.Environment (getArgs)
import IO
import NMF
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  case args of
    [fromFile] -> do
      tsv <- readTSV <$> readFile fromFile
      case tsv of
        Left err -> print err
        Right tsv@TSV {..} -> do
          let (n, m) = dim tsv
              v      = A.fromList (Z:.n:.m) (concat values)

          print . A.toList $ I.run (nmf (use v))

    _ -> putStrLn "Please provide an input file!"
