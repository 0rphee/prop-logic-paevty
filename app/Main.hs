{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.IO qualified as TIO
import Lib

main :: IO ()
main = TIO.putStrLn $ runApp "((not (P and Q)) then ((R or S) bithen not T))"
