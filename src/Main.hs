module Main where

import qualified Model.Static as S
import qualified Model.Adaptive as A

import Encoder
import Decoder
import qualified Data.Map as M

import System.Environment

usage prog = 
  mconcat ["USAGE: " ++ prog ++ " -[s|a|d] SRC DST\n"
          ,"\t-s encode with a static model\n"
          ,"\t-a encode with an adaptive model\n"
          ,"\t-d decode\n"]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of 
    ["-s", src, dst] -> do
      staticModel <- S.fromFile src
      encodeFile staticModel src dst
    ["-a", src, dst] -> do
      encodeFile A.adaptiveModel src dst
    ["-d", src, dst] -> do
      decodeFile src dst
    _ -> putStrLn (usage prog)
