{-# LANGUAGE OverloadedStrings #-}

module Main where

import Result

import CSDC.Parser

import Data.Text.Prettyprint.Doc

import Reflex.Dom

import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as ByteString

--------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithCss css $ do
  press <- divClass "button-holder" $ button "Compile"

  divClass "panel-holder" $ do

    decls <- declarationInput

    let
      declsEv = tagPromptlyDyn (_textArea_value decls) press

      handle = either Text.pack showResult . parseCSDC . Text.unpack

    defs <- holdDyn "" $ fmap handle declsEv

    divClass "def-holder" $ dynText defs

hslash :: Doc ann
hslash = pretty (replicate 60 '=')

declarationInput :: MonadWidget t m => m (TextArea t)
declarationInput = textArea config
  where
    config = TextAreaConfig "" never $ constDyn attrs
    attrs = "class" =: "decl-holder"

css :: ByteString.ByteString
css = ByteString.unlines
  [ ".panel-holder { padding-top: 20px; width: 100%; height: 600px; display: flex; }"
  , ".decl-holder { margin: auto; width: 45%; height: 100%; border-style: solid; border-width: 2px; font-family: monospace; resize: none; }"
  , ".def-holder { width: 45%; margin: auto; height: 100%; border-style: solid; border-width: 2px; overflow: scroll; white-space: pre-line; font-family: monospace; }"
  ]
