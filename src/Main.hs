module Main where

import MyPrelude

import qualified Token
--import Token (Token)
import qualified AST
--import AST (AST)
import qualified Name
--import Name (Name)

andThen :: (Show b, Show e) => IO (Maybe a) -> (a -> Either e b) -> IO (Maybe b)
andThen getPrev process = do
    prev <- getPrev
    case prev of
        Nothing -> return Nothing
        Just result -> do
            let processed = process result
            print processed
            return (right processed)

main :: IO ()
main = do
    let readInput = do
           input <- getContents
           return (Just input)
    void $ readInput      `andThen`
           Token.tokenize `andThen`
           AST.parse      `andThen`
           Name.resolveNames
