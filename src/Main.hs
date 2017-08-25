module Main where

import MyPrelude

import qualified Token
--import Token (Token)
import qualified AST
--import AST (AST)
import qualified Name
--import Name (Name)
import qualified Type
--import Type (Type)
import qualified IR

thenTry :: (Show b, Show e) => IO (Maybe a) -> (a -> Either e b) -> IO (Maybe b)
thenTry getPrev process = do
    prev <- getPrev
    case prev of
        Nothing -> return Nothing
        Just result -> do
            let processed = process result
            prettyPrint processed
            return (right processed)

main :: IO ()
main = do
    void $ fmap Just getContents `thenTry`
           Token.tokenize        `thenTry`
           AST.parse             `thenTry`
           Name.resolveNames     `thenTry`
           Type.checkTypes       `thenTry`
           (Right @() . IR.translate)
