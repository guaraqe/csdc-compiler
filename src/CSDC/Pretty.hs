{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CSDC.Pretty
  ( Render (..)
  , renderPrint
  ) where

import CSDC.Types.Name
import CSDC.Types.Annotated
import CSDC.Definition
import CSDC.Compiler

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text.Prettyprint.Doc hiding (list)
import Data.Text.Prettyprint.Doc.Render.String

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------

class Render a where
  render :: a -> Doc ann

renderPrint :: Render a => a -> IO ()
renderPrint = putStrLn . renderString . layoutPretty opts . render
  where
    opts = LayoutOptions Unbounded

--------------------------------------------------------------------------------

instance Render Name where
  render = hsep . fmap pretty . getName

--------------------------------------------------------------------------------

instance Render (Annotated Name) where
  render (Annotated list mandatory value) =
    let
      prefix =
        case mandatory of
          IsMandatory -> "*"
          NotMandatory -> ""
      suffix =
        case list of
          IsList -> "(s)"
          NotList -> ""
    in
      prefix <> render value <> suffix

--------------------------------------------------------------------------------

showDefinition :: Definition -> Doc ann
showDefinition (RecordDefinition fields) =
  "Record" <+> renderList fields
showDefinition (ChoiceDefinition choiceType choices) =
  pretty (show choiceType)  <> "Choice" <+> renderList choices
showDefinition (Equality name) =
  "Equality" <+> "{" <+> render name <+> "}"
showDefinition Predefined =
  "Predefined"

renderList :: Render a => [a] -> Doc ann
renderList = encloseSep "{ " " }" ", " . fmap render

instance Render Definition where
  render = showDefinition

--------------------------------------------------------------------------------

instance Render (Map Name Definition) where
  render = Map.foldMapWithKey prettySingle
    where
      prettySingle k v = render k <+> "→" <> softline <> render v <> line <> line

--------------------------------------------------------------------------------

instance Render DefinitionSet where
  render (DefinitionSet m) = Map.foldMapWithKey prettySingle m
    where
      prettySingle k v =
        vsep (fmap (\u -> render k <+> "→" <> softline <> render u) (Set.toList v)) <> line <> line

instance Render (Set Name) where
  render = vsep . fmap render . Set.toList
