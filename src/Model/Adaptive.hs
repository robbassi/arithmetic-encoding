module Model.Adaptive where

import Model.Types

import Control.Monad

import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import qualified Data.Map as M

data AdaptiveModel =
  AdaptiveModel { amFreqs :: Freqs
                , amDenom :: CodeValue }
  deriving (Eq, Show)

instance Model AdaptiveModel where
  update = updateAdaptiveModel
  freqs = amFreqs
  denom = amDenom
  write _ h = writeAdaptiveModel h

adaptiveModel :: AdaptiveModel
adaptiveModel = AdaptiveModel freqs denom
  where (freqs, denom) = computeCummFreqs M.empty True
 
updateAdaptiveModel :: AdaptiveModel -> Sym -> AdaptiveModel
updateAdaptiveModel m@(AdaptiveModel freqs denom) sym
  | denom < maxFreq = 
      AdaptiveModel { amFreqs = foldl incrementFreqs freqs' [sym+1..eom]
                    , amDenom = (succ denom) }
  | otherwise = m
  where incrementFreqs m s = 
          let (l, h) = getProb m s
          in M.insert s (Freq (succ l) (succ h)) m
        freqs' =
          let (l, h) = getProb freqs sym
          in M.insert sym (Freq l (succ h)) freqs

writeAdaptiveModel :: Handle -> IO ()
writeAdaptiveModel h = replicateM_ rowBytes writeZero
  where writeZero = hPutChar h '\0'

