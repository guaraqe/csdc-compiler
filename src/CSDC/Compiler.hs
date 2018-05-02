module CSDC.Compiler
  ( DefinitionSet (..)
  , compilation
  , multipleDefinitions
  , notDefined
  ) where

import CSDC.Types.Annotated
import CSDC.Types.Name
import CSDC.Types.Tree
import CSDC.Types.Choice
import CSDC.Definition

import qualified CSDC.Declaration as D

import Data.Bifunctor
import Data.Monoid

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------

newtype DefinitionSet = DefinitionSet (Map Name (Set Definition))

instance Monoid DefinitionSet where
  mempty = DefinitionSet Map.empty
  mappend (DefinitionSet m) (DefinitionSet n) =
    DefinitionSet $ Map.unionWith Set.union m n

(=:) :: Name -> Definition -> DefinitionSet
(=:) a b = DefinitionSet $ Map.singleton a (Set.singleton b)

--------------------------------------------------------------------------------

compilation :: D.CSDC -> DefinitionSet
compilation (D.CSDC outs) = foldMap compileOuter outs

--------------------------------------------------------------------------------

compileOuter :: D.Outer -> DefinitionSet

compileOuter (D.Outer constructor name inner) =
  compileInner (D.Inner constructor name inner)

compileOuter (D.DAOofDAOs name (D.Fields fields) tree) =
  let
    makeRecord a rs = Set.singleton $ RecordDefinition $
      fmap readAnnotatedName rs <>
      fmap (readAnnotatedWith a) fields
  in
    DefinitionSet $ treeMap makeName makeRecord (D.Branch name tree)

compileOuter (D.Committees name list) =
  let
    addName (Annotated _ m (Name ns)) = Annotated NotList m (Name (name:ns))
    anns = fmap readAnnotatedName list
    board = readAnnotatedName "board"
  in
    makeName name =: RecordDefinition (board : fmap addName anns) <>
    foldMap
      (\n -> (Name [name] <> annotated_name n) =: RecordDefinition [board,n])
      anns

compileOuter (D.Alias name list) =
  foldMap (\n -> makeName n =: Equality (makeName name)) list

compileOuter (D.Predefined list) =
  foldMap (\n -> makeName n =: Predefined) list

--------------------------------------------------------------------------------

compileInner :: D.Inner -> DefinitionSet
compileInner (D.End _) = mempty
compileInner (D.Inner name constructor inners) =
  let
    fields = map innerName inners
    compiled = case constructor of
      D.Record ->
        makeName name =: RecordDefinition fields
      D.Choice choice -> case choice of
        D.Enum ->
          makeName name =: ChoiceDefinition One fields
        D.OneChoice ->
          makeName name =: ChoiceDefinition One fields
        D.MultiChoice ->
          makeName name =: ChoiceDefinition Multi fields
  in
    compiled <> foldMap compileInner inners

innerName :: D.Inner -> Annotated Name
innerName (D.End s) = readAnnotatedName s
innerName (D.Inner s _ _) = readAnnotatedName s

--------------------------------------------------------------------------------

multipleDefinitions :: DefinitionSet -> (Map Name Definition, DefinitionSet)
multipleDefinitions (DefinitionSet m) =
  bimap (fmap fromSingleton) DefinitionSet (Map.partition ((==) 1 . Set.size) m)
  where
    fromSingleton = Set.elemAt 0

--------------------------------------------------------------------------------

isDefinedIn :: Set Name -> Name -> Maybe Name
isDefinedIn _ (Name []) = Nothing
isDefinedIn set name@(Name (_:ns)) =
  if Set.member name set
    then Just name
    else isDefinedIn set (Name ns)

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
