module Main where

import MyPrelude
import qualified Token as T
import qualified AST
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

{-
FIRST
  two types: Int, Bool
  arithmetic and comparison operators
  `if`. `while`? `forever`/`break`? as expressions?
  `let`
  `read()`, `write()`
  mutation (assignment)? type annotations? strings?
  no functions yet
-}

example :: Text
example =
    "var n = 0"          ++ "\n" ++
    "forever {"          ++ "\n" ++
    "    write(n)"       ++ "\n" ++
    "    let m = read()" ++ "\n" ++
    "    if m == 0 {"    ++ "\n" ++
    "        return"     ++ "\n" ++
    "    }"              ++ "\n" ++
    "    n = n + m"      ++ "\n" ++
    "}"

main :: IO ()
main = do
    a <- Text.getContents
    print (T.tokenize a)
