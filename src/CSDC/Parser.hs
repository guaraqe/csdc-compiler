module CSDC.Parser
  ( parseCSDC
  ) where

import CSDC.Parser.AbsCSDC
import CSDC.Parser.ErrM
import CSDC.Parser.ParCSDC
  ( pCSDC
  , myLexer )


parseCSDC :: String -> Either String CSDC
parseCSDC s =
  let
    tokens = myLexer s
  in
    case pCSDC tokens of
      Bad a -> Left a
      Ok a -> Right a
