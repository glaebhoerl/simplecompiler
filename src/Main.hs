module Main where

import MyPrelude

import qualified Data.Text.Prettyprint.Doc.Render.Terminal as P
import qualified LLVM.Context as L
import qualified LLVM.Module  as L
import qualified LLVM.Target  as L

import qualified Token
--import Token (Token)
import qualified AST
--import AST (AST)
import qualified Name
--import Name (Name)
import qualified Type
--import Type (Type)
import qualified IR
import qualified LLVM

thenTry :: (Show b, Show e) => IO (Maybe a) -> (a -> Either e b) -> IO (Maybe b)
thenTry getPrev process = do
    prev <- getPrev
    case prev of
        Nothing -> return Nothing
        Just result -> do
            let processed = process result
            --prettyPrint processed
            return (right processed)

main :: IO ()
main = do
    result <- fmap Just getContents `thenTry`
              Token.tokenize        `thenTry`
              AST.parse             `thenTry`
              Name.resolveNames     `thenTry`
              Type.checkTypes
    forM_ result $ \typedAST -> do
        assertM (Name.validate typedAST)
        assertM (Type.validate typedAST)
        let ir      = IR.translate typedAST
        let printIR = P.putDoc . fmap (ansiStyle . IR.defaultStyle) . IR.render
        printIR ir
        assertM (IR.validate ir)
        let ir2 = IR.eliminateTrivialBlocks ir
        printIR ir2
        assertM (IR.validate ir2)
        let astModule = LLVM.translate ir2
        L.withContext $ \context ->
            L.withModuleFromAST context astModule $ \compiledModule ->
                L.withHostTargetMachine $ \target ->
                    L.writeObjectToFile target (L.File "a.out") compiledModule



ansiStyle :: IR.Style -> P.AnsiStyle
ansiStyle IR.Style { IR.color, IR.isDull, IR.isBold, IR.isItalic, IR.isUnderlined } = style where
    style     = maybe mempty (fromColor . mapColor) color ++ fontStyle
    fontStyle = mconcat (catMaybes [justIf isBold P.bold, justIf isItalic P.italicized, justIf isUnderlined P.underlined])
    fromColor = if isDull then P.colorDull else P.color
    mapColor  = \case
        IR.Black   -> P.Black
        IR.White   -> P.White
        IR.Red     -> P.Red
        IR.Green   -> P.Green
        IR.Blue    -> P.Blue
        IR.Cyan    -> P.Cyan
        IR.Magenta -> P.Magenta
        IR.Yellow  -> P.Yellow