module Main where

import MyPrelude

import Control.Exception  (SomeException, catch)
import Data.List          (partition, isPrefixOf)
import System.Environment (getArgs)

import qualified System.Directory as Directory
import qualified System.Exit      as Exit
import qualified System.Process   as Process
import qualified Foreign.Ptr      as Ptr
import qualified Data.ByteString  as ByteString

import qualified LLVM.Analysis        as L
import qualified LLVM.Context         as L
import qualified LLVM.ExecutionEngine as L
import qualified LLVM.Module          as L
import qualified LLVM.OrcJIT          as L
import qualified LLVM.PassManager     as L
import qualified LLVM.Target          as L
import qualified LLVM.Transforms      as L

import qualified Pretty
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

tokens :: Command [With Loc Token]
tokens = do
    input <- getInput
    try (mapLeft prettyShow (Token.tokenize input))

ast :: Command (AST Loc Text)
ast = do
    input <- tokens
    try (mapLeft prettyShow (AST.parse input))

names :: Command (AST Loc ResolvedName)
names = do
    input  <- ast
    result <- try (mapLeft prettyShow (Name.resolveNames  input))
    _      <- try (mapLeft prettyShow (Name.validateNames result))
    return result

types :: Command (AST Loc TypedName)
types = do
    input  <- names
    result <- try (mapLeft prettyShow (Type.checkTypes    input))
    _      <- try (mapLeft prettyShow (Name.validateNames result)) -- make sure types are assigned to names consistently!
    _      <- try (mapLeft prettyShow (Type.validateTypes result))
    return result

ir :: Command [IR.Function]
ir = do
    result <- liftM (map (IR.translateFunction . nodeWithout)) types
    _      <- try (mapLeft prettyShow (IR.validate result))
    opt    <- liftM optimize arguments
    if opt
        then do
            let optimized = map (modify (field @"functionBody") IR.eliminateTrivialBlocks) result
            _ <- try (mapLeft prettyShow (IR.validate optimized))
            return optimized
        else do
            return result

llvmAst :: Command LLVM.Module
llvmAst = liftM LLVM.translateFunctions ir

llvmContextAndModule :: Command (L.Context, L.Module)
llvmContextAndModule = do
    moduleAst <- llvmAst
    context   <- usingManaged L.withContext
    module'   <- usingManaged (L.withModuleFromAST context moduleAst)
    liftIO (L.verify module') -- TODO is there an exception to catch??
    whenM (liftM optimize arguments) do -- FIXME if we generate invalid LLVM, we want to print it before verifying, otherwise after!
        passManager <- usingManaged (L.withPassManager (L.PassSetSpec [L.PromoteMemoryToRegister] Nothing Nothing Nothing))
        _ <- liftIO (L.runPassManager passManager module')
        return ()
    return (context, module')

llvmModule :: Command L.Module
llvmModule = liftM snd llvmContextAndModule

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
    liftIO (L.moduleObject target module')

build :: Command ()
build = do
    Arguments { inFile, outFile } <- arguments
    let objFile = textToString (fromMaybe "stdin" inFile) ++ ".o"
    module' <- llvmModule
    target  <- usingManaged L.withHostTargetMachine
    let removeOrIgnore file = liftIO (catch (Directory.removeFile file) (\(_ :: SomeException) -> return ())) -- ugh
    usingManaged \body -> do
        L.writeObjectToFile target (L.File objFile) module'
        result <- body ()
        removeOrIgnore objFile
        return result
    let args = objFile : maybe [] (prepend "-o" . single . textToString) outFile
    (exitCode, out, err) <- liftIO (Process.readProcessWithExitCode "gcc" args "")
    removeOrIgnore objFile
    when (exitCode != Exit.ExitSuccess) do
        throwError (stringToText ("GCC reported error:\n" ++ out ++ "\n" ++ err))

{- TODO port to new API
-- OrcJIT version, fails to resolve `printf` symbol
runOrcJit :: Command Text
runOrcJit = do
    let resolver :: L.MangledSymbol -> L.IRCompileLayer l -> L.MangledSymbol -> IO L.JITSymbol
        resolver _testFunc compileLayer symbol = L.findSymbol compileLayer symbol True
        nullResolver :: L.MangledSymbol -> IO L.JITSymbol
        nullResolver s = return (L.JITSymbol 0 (L.JITSymbolFlags False False))
    module'      <- llvmModule
    target       <- usingManaged L.withHostTargetMachine
    objectLayer  <- usingManaged L.withObjectLinkingLayer
    compileLayer <- usingManaged (L.withIRCompileLayer objectLayer target)
    testFunc     <- liftIO (L.mangleSymbol compileLayer "main")
    --moduleSet    <- usingManaged (L.withModuleSet compileLayer [module'] (L.SymbolResolver (resolver testFunc compileLayer) nullResolver))
    mainSymbol   <- liftIO (L.mangleSymbol compileLayer "main")
    jitSymbol    <- liftIO (L.findSymbol compileLayer mainSymbol True)
    result       <- (liftIO . runMainPtr . Ptr.castPtrToFunPtr . Ptr.wordPtrToPtr . L.jitSymbolAddress) jitSymbol
    return ("EXIT CODE: " ++ showText result)
-}

run :: Command ()
run = do
    Arguments { outFile } <- arguments
    when (isJust outFile) do
        throwError "An output file doesn't make sense for the `run` command!"
    (context, module') <- llvmContextAndModule
    engine             <- usingManaged (L.withMCJIT context Nothing Nothing Nothing Nothing)
    compiledModule     <- usingManaged (L.withModuleInEngine engine module')
    maybeMain          <- liftIO (L.getFunction compiledModule "main")
    mainPtr            <- maybe (throwError "ERROR: `main` not found in JIT-compiled code!") return maybeMain
    result             <- (liftIO . runMainPtr . Ptr.castFunPtr) mainPtr
    liftIO (putStrLn ("EXIT CODE: " ++ showText result))

foreign import ccall "dynamic" runMainPtr :: Ptr.FunPtr (IO Int32) -> IO Int32

outputCommand :: Pretty.Render a => Command a -> Command ()
outputCommand command = do
    Arguments { outFile } <- arguments
    handle <- case outFile of
        Nothing       -> return stdout
        Just fileName -> usingManaged (withFile (textToString fileName) WriteMode) -- TODO exceptions :\
    result <- command
    liftIO (Pretty.output handle result)

commands :: [(Text, Command ())]
commands = execWriter do
    let command name cmd = tell [(name, outputCommand cmd)]
    command "tokens" tokens
    command "ast"    ast
    command "names"  names
    command "types"  types
    command "ir"     ir
    command "llvm"   llvm
    command "asm"    asm
    command "obj"    obj
    -- these have their own ways to handle output
    tell [("build", build)]
    tell [("run",   run)]

main :: IO ()
main = runManaged do
    result <- (runExceptT . runCommand) do
        Arguments { command } <- arguments
        fromMaybe (throwError "Command not recognized!") (lookup command commands)
    either (liftIO . hPutStrLn stderr) return result
