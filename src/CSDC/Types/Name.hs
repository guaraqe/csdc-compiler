{-# LANGUAGE
  GeneralizedNewtypeDeriving
#-}

module CSDC.Types.Name
  ( Name (..)
  ) where

import Data.String

newtype Name = Name { getName :: [String] }
  deriving (Show, Eq, Ord, Monoid)

instance IsString Name where
  fromString = Name . words
