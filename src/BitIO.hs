module BitIO where

import Data.Bits
import Data.Word
import Data.Char (chr)
import System.IO
import Control.Concurrent.MVar

data Bit = Bit0 | Bit1 | EOF
  deriving (Eq, Show)

data BitMode = ReadBit | WriteBit
  deriving (Eq, Show)

data ReadHandle_ = 
  ReadHandle_ { rhSize :: Word8
              , rhBuff :: Word8 }

data WriteHandle_ = 
  WriteHandle_ { whMask :: Word8
               , whBuff :: Word8 }

data BitHandle = ReadHandle Handle !(MVar ReadHandle_)
               | WriteHandle Handle !(MVar WriteHandle_)

instance Show BitHandle where
  show (ReadHandle h _) = "BitIO.ReadHandle" ++ show h
  show (WriteHandle h _) = "BitIO.WriteHandle" ++ show h

openBitFile :: FilePath -> BitMode -> IO BitHandle
openBitFile p ReadBit = do
  hand <- openBinaryFile p ReadMode
  rh <- newMVar (ReadHandle_ 0 0)
  return (ReadHandle hand rh)
openBitFile p WriteBit = do
  hand <- openBinaryFile p WriteMode
  wh <- newMVar (WriteHandle_ 0x80 0)
  return (WriteHandle hand wh)

bHandle :: BitHandle -> Handle
bHandle (WriteHandle h _) = h
bHandle (ReadHandle h _) = h

bGetBit :: BitHandle -> IO Bit
bGetBit (WriteHandle _ _) = 
  error "readBit: illegal operation (handle is not open for writing)"
bGetBit (ReadHandle h mv) = do
  rh@(ReadHandle_ size buff) <- takeMVar mv
  if size == 0
    then do
      isEOF <- hIsEOF h
      if isEOF
      then putMVar mv rh *> return EOF
      else nextByte >>= update rh 8
    else do
      update rh size buff
  where nextByte = do
          c <- hGetChar h
          return $ fromIntegral (fromEnum c)
        update rh size buff = do
          putMVar mv (ReadHandle_ (pred size) (buff `shiftL` 1))
          return (if buff .&. 0x80 == 0 
                  then Bit0
                  else Bit1)

bPutBit :: BitHandle -> Bit -> IO ()
bPutBit (ReadHandle _ _) _ = 
  error "writeBit: illegal operation (handle is not open for reading)"
bPutBit _ EOF = return ()
bPutBit (WriteHandle h mv) bit = do
  (WriteHandle_ mask buff) <- takeMVar mv
  let buff' = if bit == Bit1
              then buff .|. mask
              else buff
      mask' = mask `shiftR` 1
  if mask' == 0
    then do
      hPutChar h $ chr $ fromIntegral buff'
      putMVar mv (WriteHandle_ 0x80 0)
    else do
      putMVar mv (WriteHandle_ mask' buff')

bClose :: BitHandle -> IO ()
bClose (ReadHandle h mv) = hClose h
bClose (WriteHandle h mv) = do
  -- ^ flush any remaining bits in the buffer
  --   before closing the handle
  wh@(WriteHandle_ mask buff) <- takeMVar mv
  if mask /= 0x80 
    then do
      hPutChar h . chr . fromIntegral $ buff
      putMVar mv (WriteHandle_ 0x80 0)
    else do
      putMVar mv wh
  hClose h

{- Test
pump :: BitHandle -> BitHandle -> IO ()
pump i@(ReadHandle _ _) o@(WriteHandle _ _) = do
  bit <- bGetBit i
  if bit == EOF
    then return ()
    else do
      bPutBit o bit 
      pump i o
pump o@(WriteHandle _ _) i@(ReadHandle _ _) = pump i o
pump _ _ = error "pump: illegal operation (handles have the same mode)"
-}
