module NMF
  where

--------------------------------------------------------------------------------
import           Data.Array.Accelerate
import qualified Data.Array.Accelerate as A
--------------------------------------------------------------------------------

type Matrix e = Array DIM2 e

--------------------------------------------------------------------------------



nmf :: Acc (Matrix Int) -> Acc (Matrix Int)
nmf = A.map (^2)
