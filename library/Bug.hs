module Bug
where

import Prelude
import Language.Haskell.TH.Syntax
import qualified Bug.Formatting as B


todo :: String -> Q Exp
todo message =
  do
    updatedMessage <- locationMessage <$> location
    reportWarning updatedMessage
    [|error updatedMessage|]
  where
    locationMessage location =
      B.prefixNullable (B.loc location ++ " " ++ "TODO") message

bug :: Q Exp
bug =
  do
    prefix <- locationPrefix <$> location
    [|error . B.prefixNullable prefix|]
  where
    locationPrefix location =
      showString (B.loc location) . showChar ' ' $ "Bug"

bottom :: Q Exp
bottom =
  do
    message <- locationMessage <$> location
    [|error message|]
  where
    locationMessage location =
      showString (B.loc location) . showChar ' ' $ "Bug: Bottom evaluated"
