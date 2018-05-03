module CSDC.Compiler
  ( DefinitionSet (..)
  , compilation
  , multipleDefinitions
  , notDefined
  ) where

import CSDC.Declaration
import CSDC.Definition
import CSDC.Types.Annotated
import CSDC.Types.Name
import CSDC.Types.Tree
import CSDC.Types.Choice

import Data.Bifunctor
import Data.Monoid

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------

-- | A DefinitionSet is just a mapping from names to sets of definitions.
-- It is wrapped we can give it a suitable Monoid instance.
newtype DefinitionSet = DefinitionSet (Map Name (Set Definition))

instance Monoid DefinitionSet where
  mempty = DefinitionSet Map.empty
  mappend (DefinitionSet m) (DefinitionSet n) =
    DefinitionSet $ Map.unionWith Set.union m n

(=:) :: Name -> Definition -> DefinitionSet
(=:) a b = DefinitionSet $ Map.singleton a (Set.singleton b)

--------------------------------------------------------------------------------

compilation :: CSDC -> DefinitionSet
compilation (CSDC outs) = foldMap compileOuter outs

--------------------------------------------------------------------------------

compileOuter :: Outer -> DefinitionSet

-- Outer is compiled as a reconstructed Inner
compileOuter (Outer constructor name inner) =
  compileInner (Inner constructor name inner)

-- DAOofDAOs reconstruct records level by level of the tree, adding the common
-- fields to all elements but the leaves.
compileOuter (DAOofDAOs name (Fields fields) tree) =
  let
    makeRecord a rs = Set.singleton $ RecordDefinition $
      fmap readAnnotatedName rs <>
      fmap (readAnnotatedWith a) fields
  in
    DefinitionSet $ treeMap makeName makeRecord (Branch name tree)

-- Committees create a record of composed of subcommittees, each with a
-- corresponding board.
compileOuter (Committees name list) =
  let
    addName (Annotated _ m (Name ns)) = Annotated NotList m (Name (name:ns))
    anns = fmap readAnnotatedName list
    board = readAnnotatedName "board"
  in
    makeName name =: RecordDefinition (board : fmap addName anns) <>
    foldMap
      (\n -> (Name [name] <> annotated_name n) =: RecordDefinition [board,n])
      anns

-- Alias are compiled to lists of equalitites.
compileOuter (Alias name list) =
  foldMap (\n -> makeName n =: Equality (makeName name)) list

-- Predefined is compiled to lists of predefineds.
compileOuter (CSDC.Declaration.Predefined list) =
  foldMap (\n -> makeName n =: CSDC.Definition.Predefined) list

--------------------------------------------------------------------------------

-- Inner trees are compiled by generating records level by level.
compileInner :: Inner -> DefinitionSet
compileInner (End _) = mempty
compileInner (Inner name constructor inners) =
  let
    fields = map innerName inners
    compiled = case constructor of
      Record ->
        makeName name =: RecordDefinition fields
      Choice choice -> case choice of
        Enum ->
          makeName name =: ChoiceDefinition One fields
        OneChoice ->
          makeName name =: ChoiceDefinition One fields
        MultiChoice ->
          makeName name =: ChoiceDefinition Multi fields
  in
    compiled <> foldMap compileInner inners

innerName :: Inner -> Annotated Name
innerName (End s) = readAnnotatedName s
innerName (Inner s _ _) = readAnnotatedName s

--------------------------------------------------------------------------------

-- | Checks for multiple definitions within the DefinitionSet.
multipleDefinitions :: DefinitionSet -> (Map Name Definition, DefinitionSet)
multipleDefinitions (DefinitionSet m) =
  bimap (fmap fromSingleton) DefinitionSet (Map.partition ((==) 1 . Set.size) m)
  where
    fromSingleton = Set.elemAt 0

--------------------------------------------------------------------------------

-- | Applies the simple hierarchy rule for checking if a name is already
-- defined.
isDefinedIn :: Set Name -> Name -> Maybe Name
isDefinedIn _ (Name []) = Nothing
isDefinedIn set name@(Name (_:ns)) =
  if Set.member name set
    then Just name
    else isDefinedIn set (Name ns)

-- | From the set of definitions, get those that are undefined, and the set
-- completed by the simple hierarchy rule
notDefined :: Map Name Definition -> (Set Name, Map Name Definition)
notDefined defs =
  second (Map.union defs) $ Set.foldl' f (Set.empty, Map.empty) names
  where
    names = allDefinitionNames defs
    defined = allDefined defs

    f x name =
      case isDefinedIn defined name of
        Nothing -> first (Set.insert name) x
        Just name' ->
          if name == name'
            then x
            else second (Map.insert name (Equality name')) x
