module Encoder where

import Model.Types
import BitIO
import Data.Bits
import System.IO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

data Encoder m = 
  Enc { low :: CodeValue
      , high :: CodeValue
      , pending :: Int
      , inFile :: Handle
      , outFile :: BitHandle
      , model :: m }
  deriving (Show)

type EncoderT m = StateT (Encoder m) IO

newEncoder :: Model m => m -> Handle -> BitHandle -> Encoder m
newEncoder m i o = Enc { low = 0
                       , high = mMax
                       , pending = 0
                       , inFile = i
                       , outFile = o
                       , model = m }

encodeFile :: Model m => m -> FilePath -> FilePath -> IO ()
encodeFile model i o = do
  inFile <- openBinaryFile i ReadMode
  outFile <- openBitFile o WriteBit
  write model (bHandle outFile)
  evalStateT encode (newEncoder model inFile outFile)
  hClose inFile
  bClose outFile

encode :: Model m => EncoderT m ()
encode = encode' *> flushPending
  where encode' = do
          enc@(Enc low high pending inFile outFile m) <- get
          isEOF <- lift (hIsEOF inFile)
          if isEOF 
            then do
              encodeSym eom
            else do
              char <- lift (hGetChar inFile)
              let sym = fromIntegral (fromEnum char)
              encodeSym sym
              updateModel sym
              encode'
        flushPending = do
          -- ^ Flush any leftover bits after encoding is done.
          enc@(Enc low _ pending _ _ m) <- get
          put $ enc { pending = succ pending }
          if low < mQuarter 
            then writeBits Bit0
            else writeBits Bit1
    
encodeSym :: Model m => Sym -> EncoderT m ()
encodeSym sym = update *> writeSym 
  where update = do
          -- ^ Initialize the Encoder state for the next symbol.
          enc@(Enc low high _ _ _ m) <- get
          let (lower, upper) = getProb (freqs m) sym
              denom' = denom m
              range = high - low + 1
              high' = low + range * upper `div` denom' - 1
              low' = low + range * lower `div` denom'
          put $ enc { low = low', high = high' }
        writeSym = do
          -- ^ Write all the bits required to encode the 
          --   current symbol.
          enc@(Enc low high pending _ _ m) <- get
          if high < mHalf 
          then writeBits Bit0 *> itr'
          else if low >= mHalf 
          then writeBits Bit1 *> itr'
          else if low >= mQuarter && high < mThreeQ 
          then itr $ enc { low = low - mQuarter
                         , high = high - mQuarter
                         , pending = succ pending }
          else return ()
        itr enc@(Enc low high _ _ _ m) = do
          -- ^ Shift `low` and `high` before the next iteration 
          --   in `writeSym`.
          put $ enc { low = (low `shiftL` 1) .&. mMax
                    , high = (high `shiftL` 1 + 1) .&. mMax  }
          writeSym
        itr' = get >>= itr

updateModel :: Model m => Sym -> EncoderT m ()
updateModel sym = do
  enc@(Enc _ _ _ _ _ m) <- get
  put $ enc { model = update m sym }

--
-- Helpers
--

writeBits :: Model m => Bit -> EncoderT m ()
writeBits EOF = return ()
writeBits bit = do
  enc@(Enc _ _ p _ o _) <- get
  lift $ do
    bPutBit o bit
    writeNBits o p bit'
  put $ enc { pending = 0 }
  where bit' = inv bit
        inv Bit0 = Bit1
        inv Bit1 = Bit0

writeNBits :: BitHandle -> Int -> Bit -> IO ()
writeNBits _ 0 _ = return ()
writeNBits o n b = do
  bPutBit o b
  writeNBits o (pred n) b


