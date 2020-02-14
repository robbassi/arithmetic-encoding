module Decoder where

import Model.Types
import Model.IO
import BitIO
import Data.Bits
import System.IO
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

data Decoder m = 
  Dec { low :: CodeValue
      , high :: CodeValue
      , value :: CodeValue
      , inFile :: BitHandle
      , outFile :: Handle
      , model :: m }
  deriving (Show)

type DecoderT m = StateT (Decoder m) IO

newDecoder :: Model m => m -> BitHandle -> Handle -> Decoder m
newDecoder m i o = Dec { low = 0
                       , high = mMax
                       , value = 0
                       , inFile = i
                       , outFile = o
                       , model = m }

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile i o = do
  inFile <- openBitFile i ReadBit
  outFile <- openBinaryFile o WriteMode
  model <- readModel (bHandle inFile)
  evalStateT decode (newDecoder model inFile outFile)
  bClose inFile
  hClose outFile

decode :: Model m => DecoderT m ()
-- ^ Initialize the Decoder and run it. This function assumes
--   the decoder has not been run already.
decode = (preload cBits) *> decode'
  where decode' = do
          dec <- get
          sym <- decodeSym
          if sym == eom
            then return ()
            else do
              let char = toEnum (fromIntegral sym)
              lift $ hPutChar (outFile dec) char
              updateModel sym
              decode'

decodeSym :: Model m => DecoderT m Sym
-- ^ Decode the next symbol, and update the state of the Decoder.
decodeSym = nextSym <* update
  where update = do
          -- ^ Compute the next `value` based on the current
          --   state of the Decoder. Some branches use `itr`
          --   to increment the state and loop back into `update`.
          dec@(Dec low high value inFile _ m) <- get
          if high < mHalf 
          then itr dec
          else if low >= mHalf 
          then itr $ dec { value = value - mHalf }
          else if low >= mQuarter && high < mThreeQ 
          then itr $ dec { low = low - mQuarter
                         , high = high - mQuarter
                         , value = value - mQuarter }
          else return ()
        itr dec@(Dec low high value inFile _ m) = do
          -- ^ Update the state of the Decoder before the 
          --   next iteration in `update`.
          bit <- lift (readBit' inFile)
          put $ dec { low = (low `shiftL` 1) .&. mMax
                    , high = (high `shiftL` 1 + 1) .&. mMax
                    , value = value `shiftL` 1 + bit }
          update

updateModel :: Model m => Sym -> DecoderT m ()
updateModel sym = do
  dec@(Dec _ _ _ _ _ m) <- get
  put $ dec { model = update m sym }

--
-- Helpers
--

preload :: Model m => Int -> DecoderT m ()
-- ^ Initialize `value` by processing the first `n` bits.
preload 0 = return ()
preload n = do
  dec@(Dec _ _ value inFile _ _) <- get
  bit <- lift $ bGetBit inFile
  case bit of
    Bit0 -> put $ dec { value = value `shiftL` 1 }
    Bit1 -> put $ dec { value = value `shiftL` 1 + 1 }
    EOF  -> error "preload: unexpected EOF"
  preload (pred n)

nextSym :: Model m => DecoderT m Sym
-- ^ Helper function to retreive the next symbol based on
--   the current state of the Decoder.
nextSym = do
  dec@(Dec low high value inFile _ m) <- get
  let denom' = denom m
      range = high - low + 1
      prob = ((value - low + 1) * denom' - 1) `div` range
      (sym, lower, upper) = getSym (freqs m) prob
      low' = low + range * lower `div` denom'
      high' = low + range * upper `div` denom' - 1
  put $ dec { low = low', high = high' }
  return sym

readBit' :: Num a => BitHandle -> IO a
-- ^ Helper function to read the next bit as a Num. Casts
--   EOF values to 0.
readBit' i = do
  b <- bGetBit i
  return $ case b of 
    EOF  -> 0
    Bit0 -> 0
    Bit1 -> 1
