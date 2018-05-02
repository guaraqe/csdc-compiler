{-# LANGUAGE LambdaCase #-}

module Main where

import Result

import CSDC.Parser
import qualified Data.Text.IO as Text

main :: IO ()
main =
  fmap parseCSDC getContents >>= \case
    Left err ->
      putStrLn err
    Right csdc ->
      Text.putStrLn (showResult csdc)
