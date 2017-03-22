module Bug.Formatting
where

import Prelude
import Language.Haskell.TH (Loc(..))


loc :: Loc -> String
loc (Loc filename package module_ (startLine, startColumn) (endLine, endColumn)) =
  builder ""
  where
    builder =
      showString package .
      showChar ':' .
      showString module_ .
      showChar ':' .
      position
      where
        position =
          case startLine == endLine of
            True ->
              shows startLine .
              showChar ':' .
              columns
              where
                columns =
                  if endColumn - startColumn > 1
                    then shows startColumn . showChar '-' . shows endColumn
                    else shows startColumn
            False ->
              lineAndColumn startLine startColumn .
              showChar '-' .
              lineAndColumn endLine endColumn
              where
                lineAndColumn line column =
                  showChar '(' .
                  shows line .
                  showChar ':' .
                  shows column .
                  showChar ')'

prefixNullable :: String -> String -> String
prefixNullable to what =
  if null what
    then to
    else showString to . showString ": " . showString what $ ""
