module CSDC.Types.Annotated
  ( IsList (..)
  , IsMandatory (..)
  , Annotated (..)
  , readAnnotatedName
  , readAnnotatedWith
  , makeName
  ) where

import CSDC.Types.Name

--------------------------------------------------------------------------------

data IsList = IsList | NotList
  deriving (Show, Eq, Ord)

isList :: String -> IsList
isList s =
  let
    n = length s - 3
  in
    if drop n s == "(s)"
      then IsList
      else NotList

unList :: IsList -> String -> String
unList NotList s = s
unList IsList s =
  let
    n = length s - 3
  in
    take n s

--------------------------------------------------------------------------------

data IsMandatory = IsMandatory | NotMandatory
  deriving (Show, Eq, Ord)

isMandatory :: String -> IsMandatory
isMandatory s =
  if head s == '*'
    then IsMandatory
    else NotMandatory

unMandatory :: IsMandatory -> String -> String
unMandatory NotMandatory s = s
unMandatory IsMandatory s = tail s

--------------------------------------------------------------------------------

data Annotated a = Annotated
  { annotated_list :: IsList
  , annotated_mandatory :: IsMandatory
  , annotated_name :: a
  } deriving (Show, Eq, Ord)

readAnnotatedName :: String -> Annotated Name
readAnnotatedName = readAnnotated (Name . words)

readAnnotated :: (String -> a) -> String -> Annotated a
readAnnotated f s =
  let
    list = isList s
    mandatory = isMandatory s
    value =
      unMandatory mandatory $
      unList list $
      s
  in
    Annotated list mandatory (f value)

readAnnotatedWith :: String -> String -> Annotated Name
readAnnotatedWith a =
  let
    f x =
      case words x of
        "self":xs -> Name (stringName a:xs)
        xs -> Name xs
  in
    readAnnotated f

--------------------------------------------------------------------------------

makeName :: String -> Name
makeName = annotated_name . readAnnotatedName

stringName :: String -> String
stringName = unwords . getName . makeName
