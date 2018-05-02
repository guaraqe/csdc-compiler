{-# LANGUAGE OverloadedStrings #-}

module Result
  ( showResult
  ) where

import CSDC.Compiler
import CSDC.Declaration
import CSDC.Pretty

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

showResult :: CSDC -> Text
showResult csdc =
  let
    compiled = compilation csdc
    (singleDef, multiDef@(DefinitionSet d)) = multipleDefinitions compiled
    (noDefs, defs) = notDefined singleDef
    opts = LayoutOptions Unbounded
  in
    renderStrict $
    layoutPretty opts $
    concatWith (\a b -> a <> line <> line <> b) $
    concat
      [ if Set.null noDefs
          then []
          else [ "NOT DEFINED:"
               , render noDefs ]
      , if Map.null d
          then []
          else [ "MULTIPLE DEFINED:"
               , render multiDef ]
      , if Set.null noDefs && Map.null d
          then
            [ "SUCCESS:"
            , render defs ]
          else
            if Map.null defs
              then []
              else
                [ "PARTIAL RESULT:"
                , render defs ]
      ]
