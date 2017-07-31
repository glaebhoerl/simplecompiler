module Main where

import MyPrelude
import qualified Token as T
import qualified AST
--import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
    a <- Text.getContents
    let tokenized = T.tokenize a
    print tokenized
    case tokenized of
        Right tokens -> print (AST.parse tokens)
        _ -> return ()
