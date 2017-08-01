module Main where

import MyPrelude
import qualified Token as T
import qualified AST

main :: IO ()
main = do
    a <- getContents
    let tokenized = T.tokenize a
    print tokenized
    case tokenized of
        Right tokens -> print (AST.parse tokens)
        _ -> return ()
