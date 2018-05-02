module CSDC.Types.Record
  ( Record (..)
  ) where

import CSDC.Types.Annotated
import CSDC.Types.Name

data Record = Record
  { record_subrecords :: [Annotated Name]
  , record_fields :: [Annotated Name]
  } deriving (Show, Eq)
