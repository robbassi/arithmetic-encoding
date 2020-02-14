module BitIO where

import Data.Bits
import Data.Word
import Data.Char (chr)
import System.IO
import Control.Concurrent.MVar
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

data Bit = Bit0 | Bit1 | EOF
  deriving (Eq, Show)

data ReaderState = 
  ReaderState { rSize :: Word8
              , rBuff :: Word8
              , rHandle :: Handle }
  deriving (Eq, Show)

type BitReader = StateT ReaderState IO

data WriterState = 
  WriterState { wMask :: Word8
              , wBuff :: Word8
              , wHandle :: Handle }
  deriving (Eq, Show)

type BitWriter = StateT WriterState IO

type BitIO = StateT (ReaderState, WriterState) IO

newReaderState :: Handle -> ReaderState
newReaderState f = 
  ReaderState { rSize = 0
              , rBuff = 0
              , rHandle = f }

newWriterState :: Handle -> WriterState
newWriterState f =
  WriterState { wMask = 0x80
              , wBuff = 0
              , wHandle = f }

newBitIO :: Handle -> Handle -> (ReaderState, WriterState)
newBitIO i o = (newReaderState i, newWriterState o)

readBit :: BitReader Bit
readBit = do
  s@(ReaderState n b f) <- get
  if n == 0
  then do
    isEOF <- liftIO (hIsEOF f)
    if isEOF
    then return EOF
    else do
      b' <- liftIO (nextByte f)
      update s b' 8
  else update s b n
  where highBit b = if b .&. 0x80 == 0 
                    then Bit0
                    else Bit1
        nextByte f = do
          c <- hGetChar f
          return $ fromIntegral (fromEnum c)
        update s b n = do
          put s { rSize = pred n
                , rBuff = b `shiftL` 1 }
          return (highBit b)
      
writeBit :: Bit -> BitWriter ()
writeBit EOF = return ()
writeBit bit = do
  s@(WriterState m b f) <- get
  let b' = if bit == Bit1
           then b .|. m
           else b
      m' = m `shiftR` 1
  if m' == 0
  then do
    liftIO $ hPutChar f $ chr $ fromIntegral b'
    put s { wMask = 0x80
          , wBuff = 0 }
  else do
    put s { wMask = m'
          , wBuff = b' }
  return ()


-- * Testing

bitPump :: BitIO ()
bitPump = do
  (rs, ws) <- get
  (bit, rs') <- liftIO $ runStateT (readBit) rs
  ws' <- liftIO $ execStateT (writeBit bit) ws
  put (rs',ws')
  if bit == EOF
  then return ()
  else bitPump

readToEOF :: Handle -> BitReader [Bit]
-- ^ for debugging only
readToEOF h = do
  bits <- loop' []
  return $ reverse bits
  where loop' bits@(EOF:_) = return bits
        loop' l = do
          b <- readBit
          loop' (b:l)


