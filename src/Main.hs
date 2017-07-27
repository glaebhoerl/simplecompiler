module Main where

import MyPrelude
import qualified Token
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


main :: IO ()
main = do
  putStrLn "hello world"
