module CSDC.Definition
  ( Definition (..)
  , allDefinitionNames
  , allDefined
  ) where

import CSDC.Types.Annotated
import CSDC.Types.Choice
import CSDC.Types.Name

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Definition =
    RecordDefinition [Annotated Name]
  | ChoiceDefinition Choice [Annotated Name]
  | Equality Name
  | Predefined
  deriving (Show, Eq, Ord)

definitionNames :: Definition -> Set Name
definitionNames (RecordDefinition ns) = Set.fromList $ fmap annotated_name ns
definitionNames (ChoiceDefinition _ ns) = Set.fromList $ fmap annotated_name ns
definitionNames (Equality n) = Set.singleton n
definitionNames Predefined = Set.empty

allDefinitionNames :: Map Name Definition -> Set Name
allDefinitionNames = Set.unions . fmap definitionNames . Map.elems

allDefined :: Map Name Definition -> Set Name
allDefined = Map.keysSet
