module CSDC.Types.Choice
  ( Choice (..)
  ) where

data Choice = One | Multi
  deriving (Show, Eq, Ord)
