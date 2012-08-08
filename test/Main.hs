module Main where

import Test.Framework (defaultMain)

import qualified GameStateTests as GST

main :: IO ()
main = defaultMain [ GST.test ]
