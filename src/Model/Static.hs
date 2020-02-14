module Model.Static where

import Model.Types

import Control.Monad (when)

import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import Data.Foldable (traverse_)
import qualified Data.Map as M

data StaticModel = 
  StaticModel { smFreqs :: Freqs
              , smDenom :: CodeValue }
  deriving (Eq, Show)

instance Model StaticModel where
  update = const
  freqs = smFreqs
  denom = smDenom
  write m h = writeStaticModel h m

fromFile :: FilePath -> IO StaticModel
fromFile f = do
  (freqs, isSample) <- withBinaryFile f ReadMode (computeFreqs M.empty maxFreq)
  let (cummFreqs, denom) = computeCummFreqs freqs isSample
  return $ StaticModel cummFreqs denom
  where computeFreqs m n f = do
          isEOF <- hIsEOF f
          if n == 0 || isEOF
            then let freqs = M.insert eom 1 m
                     isSample = n == 0 && not isEOF
                 in return (freqs, isSample)
            else do
              c <- hGetChar f
              let sym = fromIntegral $ fromEnum c
              computeFreqs (M.insertWith (+) sym 1 m) (pred n) f

writeStaticModel :: Handle -> StaticModel -> IO ()
writeStaticModel h (StaticModel f _) = traverse_ write [0..eom] 
  where write sym = do
          let (lower, upper) = getProb f sym
              delta = upper - lower
          when (delta > 0)
               (writeValue sym symBytes 
             *> writeValue delta codeBytes)
        writeValue v s = with v (\p -> hPutBuf h p s)


