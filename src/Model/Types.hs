{-# LANGUAGE NamedFieldPuns, ExistentialQuantification #-}
module Model.Types where

import Data.Bits
import Data.Word
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.IO (Handle)

type Sym = Word16
type CodeValue = Word32
type Freqs = M.Map Sym Freq

data Freq = Freq { fLower :: CodeValue
                 , fUpper :: CodeValue }
  deriving (Eq, Show)

class Model m where
  update :: m -> Sym -> m
  freqs :: m -> Freqs
  denom :: m -> CodeValue
  write :: m -> Handle -> IO ()

data ModelBox = forall m. Model m => ModelBox m
-- ^ Existenstial type for all instances of `Model`.

instance Model ModelBox where
  update (ModelBox m) s = ModelBox $ update m s
  freqs (ModelBox m) = freqs m
  denom (ModelBox m) = denom m
  write (ModelBox m) h = write m h

--
-- Constants
--

maxSym :: Sym
-- ^ Bytes 0 - 255 + eom
maxSym = 257

eom :: Sym
-- ^ End of message symbol
eom = maxSym - 1

bits :: Int
-- ^ Total bits available for encoding
bits = finiteBitSize (0 :: CodeValue)

cBits :: Int
-- ^ Bits available for the `CodeValue`
cBits = bits `div` 2

fBits :: Int
-- ^ Bits available for the frequency
fBits = cBits - 2

maxFreq :: CodeValue
-- ^ Maximum possible frequency based on `fBits`
maxFreq = 1 `shiftL` fBits

mMax :: CodeValue
-- ^ High bit mask
mMax = 1 `shiftL` cBits - 1

mThreeQ :: CodeValue
-- ^ Three quarter bit mask
mThreeQ = mQuarter * 3

mHalf :: CodeValue
-- ^ Half bit mask
mHalf = mQuarter * 2

mQuarter :: CodeValue
-- ^ Quarter bit mask
mQuarter = mMax `div` 4 + 1

codeBytes :: Int
-- ^ Bytes required for `CodeValue`
codeBytes = bits `div` 8

symBytes :: Int
-- Bytes required for `Sym`
symBytes = finiteBitSize (0 :: Sym) `div` 8

rowBytes :: Int
-- ^ Bytes for one header row.
rowBytes = symBytes + codeBytes

--
-- Helpers
--

computeCummFreqs :: M.Map Sym CodeValue -> Bool -> (Freqs, CodeValue)
-- ^ Convert a map of frequencies to cummulative frequencies. When `isSample`
--   is True, the frequencies are incomplete, and the default frequency is 1.
computeCummFreqs freqs isSample = foldl merge (M.empty, 0) [0..maxSym]
  where merge (m,l) k = (M.insert k (Freq l l') m, l')
          where l' = l + f
                f = fromMaybe baseFreq $ M.lookup k freqs
        baseFreq | isSample = 1
                 | otherwise = 0

getProb :: Freqs -> Sym -> (CodeValue, CodeValue)
-- ^ Find the frequency of the given symbol.
getProb freqs sym = case M.lookup sym freqs of
  Nothing -> error $ "getProb: unexpected symbol '" ++ show sym ++ "'"
  Just (Freq lower upper) -> (lower, upper)

getSym :: Freqs -> CodeValue -> (Sym, CodeValue, CodeValue)
-- ^ Find the symbol the matches a given `CodeValue`.
getSym freqs prob = go 0
  where go n = let (lower, upper) = getProb freqs n
               in if prob < upper
                 then (n, lower, upper)
                 else go (succ n)

