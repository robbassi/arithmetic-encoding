module Model.IO where

import Model.Types
import Model.Static (StaticModel(..))
import Model.Adaptive (adaptiveModel)

import Control.Monad

import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import qualified Data.Map as M

readModel :: Handle -> IO ModelBox
-- ^ Read a `Model` from a `Handle`. The value is wrapped in `ModelBox`
--   since it's type isn't known until runtime.
readModel h = do
  adaptive <- isAdaptive h
  if adaptive
    then do
      box adaptiveModel
    else do
      freqs <- readFreqs h M.empty
      let (freqs', denom) = computeCummFreqs freqs False
      box (StaticModel freqs' denom)
  where box m = return (ModelBox m)

isAdaptive :: Handle -> IO Bool
isAdaptive h = do
  -- ^ Check if the file was encoded with an `AdaptiveModel`.
  header <- replicateM rowBytes (hGetChar h)
  let adaptive = all (== '\0') header
  unless adaptive
         (hSeek h AbsoluteSeek 0)
  return adaptive

readFreqs :: Handle -> M.Map Sym CodeValue -> IO (M.Map Sym CodeValue)
readFreqs h m = do
  -- ^ Read all the frequencies until `eom`.
  sym <- readValue h symBytes
  freq <- readValue h codeBytes
  let m' = M.insert sym freq m
  if sym == eom
    then return m'
    else readFreqs h m'

readValue :: Storable a => Handle -> Int -> IO a
readValue h s = do
  -- ^ Read fixed number of bytes.
  guardEOF h
  p <- malloc
  hGetBuf h p s
  x <- peek p
  free p
  return x

guardEOF :: Handle -> IO ()
guardEOF h = do
  -- Fail on EOF, the model is broken.
  isEOF <- hIsEOF h
  when isEOF $ do
    hClose h 
    error "readModel: invalid model (hit end of file)"
