module Main where

import MyPrelude

import Control.Exception  (SomeException, catch)
import Data.List          (partition, isPrefixOf)
import System.Environment (getArgs)

import qualified System.Directory                          as Directory
import qualified System.Exit                               as Exit
import qualified System.Process                            as Process
import qualified Data.ByteString                           as ByteString
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty

import qualified LLVM.Analysis    as L
import qualified LLVM.Context     as L
import qualified LLVM.Module      as L
import qualified LLVM.PassManager as L
import qualified LLVM.Target      as L
import qualified LLVM.Transforms  as L

import qualified Token
import qualified AST
import qualified Name
import qualified Type
import qualified IR
import qualified LLVM

import Token (Token)
import AST   (AST)
import Name  (ResolvedName)
import Type  (TypedName)

newtype Command a = Command {
    runCommand :: ExceptT Text Managed a
} deriving (Functor, Applicative, Monad, MonadIO, MonadManaged, MonadError Text)

data Arguments = Arguments {
    command  :: Text,
    inFile   :: Maybe Text,
    outFile  :: Maybe Text,
    optimize :: Bool
} deriving Generic

arguments :: Command Arguments
arguments = do
    (flags, args) <- liftM (partition (isPrefixOf "-")) (liftIO getArgs)
    let optimize = elem "-O" flags -- TODO warn on unknown flags
    (command, inFile, outFile) <- case map stringToText args of
        []        -> throwError "No command given!" -- TODO print help
        [a]       -> return (a, Nothing, Nothing)
        [a, b]    -> return (a, Just b,  Nothing)
        [a, b, c] -> return (a, Just b,  Just c)
        _         -> throwError "Too many arguments!"
    return Arguments { command, inFile, outFile, optimize }

getInput :: Command Text
getInput = do
    Arguments { inFile } <- arguments
    case inFile of
        Nothing       -> liftIO getContents
        Just fileName -> liftIO (readFile (textToString fileName)) -- TODO catch exceptions :\

tokens :: Command [Token]
tokens = do
    input <- getInput
    try (mapLeft prettyShow (Token.tokenize input))

ast :: Command (AST Text)
ast = do
    input <- tokens
    try (mapLeft prettyShow (AST.parse input))

names :: Command (AST ResolvedName)
names = do
    input  <- ast
    result <- try (mapLeft prettyShow (Name.resolveNames input))
    _      <- try (mapLeft prettyShow (Name.validate     result))
    return result

types :: Command (AST TypedName)
types = do
    input  <- names
    result <- try (mapLeft prettyShow (Type.checkTypes input))
    _      <- try (mapLeft prettyShow (Type.validate   result))
    return result

ir :: Command IR.Block
ir = do
    result <- liftM IR.translate types
    _      <- try (mapLeft prettyShow (IR.validate result))
    opt    <- liftM optimize arguments
    if opt
        then do
            let optimized = IR.eliminateTrivialBlocks result
            _ <- try (mapLeft prettyShow (IR.validate optimized))
            return optimized
        else do
            return result

llvmAst :: Command LLVM.Module
llvmAst = liftM LLVM.translate ir

llvmModule :: Command L.Module
llvmModule = do
    moduleAst <- llvmAst
    context   <- usingManaged L.withContext
    module'   <- usingManaged (L.withModuleFromAST context moduleAst)
    liftIO (L.verify module') -- TODO is there an exception to catch??
    whenM (liftM optimize arguments) $ do -- FIXME if we generate invalid LLVM, we want to print it before verifying, otherwise after!
        passManager <- usingManaged (L.withPassManager (L.PassSetSpec [L.PromoteMemoryToRegister] Nothing Nothing Nothing))
        _ <- liftIO (L.runPassManager passManager module')
        return ()
    return module'

llvm :: Command ByteString
llvm = do
    module' <- llvmModule
    liftIO (L.moduleLLVMAssembly module')

asm :: Command ByteString
asm = do
    module' <- llvmModule
    target  <- usingManaged L.withHostTargetMachine
    liftIO (L.moduleTargetAssembly target module')

obj :: Command ByteString
obj = do
    module' <- llvmModule
    target  <- usingManaged L.withHostTargetMachine
    liftIO (L.moduleTargetAssembly target module')

build :: Command ()
build = do
    Arguments { inFile, outFile } <- arguments
    let objFile = textToString (fromMaybe "stdin" inFile) ++ ".o"
    module' <- llvmModule
    target  <- usingManaged L.withHostTargetMachine
    let removeOrIgnore file = liftIO (catch (Directory.removeFile file) (\(_ :: SomeException) -> return ())) -- ugh
    using $ managed_ $ \body -> do
        L.writeObjectToFile target (L.File objFile) module'
        result <- body
        removeOrIgnore objFile
        return result
    let args = objFile : maybe [] (prepend "-o" . single . textToString) outFile
    (exitCode, out, err) <- liftIO (Process.readProcessWithExitCode "gcc" args "")
    removeOrIgnore objFile
    when (exitCode != Exit.ExitSuccess) $ do
        throwError (stringToText ("GCC reported error:\n" ++ out ++ "\n" ++ err))

run :: Command ()
run = todo

outputCommand :: Output a => Command a -> Command ()
outputCommand command = do
    Arguments { outFile } <- arguments
    handle <- case outFile of
        Nothing       -> return stdout
        Just fileName -> usingManaged (withFile (textToString fileName) WriteMode) -- TODO exceptions :\
    result <- command
    liftIO (output handle result)

commands :: [(Text, Command ())]
commands = execWriter $ do
    let command name cmd = tell [(name, outputCommand cmd)]
    command "tokens" tokens
    command "ast"    ast
    command "names"  names
    command "types"  types
    command "ir"     ir
    command "llvm"   llvm
    command "asm"    asm
    command "obj"    obj
    -- these have their own way to handle output
    tell [("build", build)]
    tell [("run",   run)]

main :: IO ()
main = runManaged $ do
    result <- (runExceptT . runCommand) $ do
        Arguments { command } <- arguments
        fromMaybe (throwError "Command not recognized!") (lookup command commands)
    either (liftIO . hPutStrLn stderr) return result

class Output a where
    output :: Handle -> a -> IO ()
    default output :: Show a => Handle -> a -> IO ()
    output handle = hPutStr handle . prettyShow

instance Output Text where
    output = hPutStr

instance Output ByteString where
    output = ByteString.hPutStr

instance Output Token -- TODO nicer rendering
instance Show a => Output [a]
instance Show a => Output (AST a) -- TODO nicer rendering
instance Output LLVM.Module

instance Output IR.Block where
    output handle = Pretty.hPutDoc handle . fmap (ansiStyle . IR.defaultStyle) . IR.render

ansiStyle :: IR.Style -> Pretty.AnsiStyle
ansiStyle IR.Style { IR.color, IR.isDull, IR.isBold, IR.isItalic, IR.isUnderlined } = style where
    style     = maybe mempty (fromColor . mapColor) color ++ fontStyle
    fontStyle = mconcat (catMaybes [justIf isBold Pretty.bold, justIf isItalic Pretty.italicized, justIf isUnderlined Pretty.underlined])
    fromColor = if isDull then Pretty.colorDull else Pretty.color
    mapColor  = \case
        IR.Black   -> Pretty.Black
        IR.White   -> Pretty.White
        IR.Red     -> Pretty.Red
        IR.Green   -> Pretty.Green
        IR.Blue    -> Pretty.Blue
        IR.Cyan    -> Pretty.Cyan
        IR.Magenta -> Pretty.Magenta
        IR.Yellow  -> Pretty.Yellow