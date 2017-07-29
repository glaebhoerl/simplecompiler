module Main where

import MyPrelude
import qualified Token as T
import qualified AST

{-
FIRST
  two types: Int, Bool
  arithmetic and comparison operators
  `if`. `while`? `forever`/`break`? as expressions?
  `let`
  `read()`, `write()`
  mutation (assignment)? type annotations? strings?
  no functions yet

Example program:
    var n = 0
    forever {
        write(n)
        let m = read()
        if m == 0 {
            return
        }
        n = n + m
    }
-}

example :: Text
example =
    "var n = 0\n" ++
    "forever {\n" ++
    "    write(n)\n" ++
    "    let m = read()\n" ++
    "    if m == 0 {\n" ++
    "        return\n" ++
    "    }\n" ++
    "    n = n + m\n" ++
    "}\n"

main :: IO ()
main = print (T.tokenize example)
