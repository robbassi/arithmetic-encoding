module Main where

import qualified Model.Static as S
import qualified Model.Adaptive as A

import Encoder
import Decoder
import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn "hello world"
