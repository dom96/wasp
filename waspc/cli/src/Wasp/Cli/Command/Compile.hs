module Wasp.Cli.Command.Compile
  ( compileIO,
    compile,
    compileWithOptions,
    compileIOWithOptions,
    defaultCompileOptions,
    formatErrorOrWarningMessages,
    printCompilerWarningsIfAny,
    printCompilerWarningsAndStatusAndThrowIfAnyErrors,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Common (WaspProjectDir)
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Lib (CompileError, CompileWarning)
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

compile :: Command ()
compile = do
  -- TODO: Consider a way to remove the redundancy of finding the project root
  -- here and in compileWithOptions. One option could be to add this to defaultCompileOptions
  -- add make externalCodeDirPath a helper function, along with any others we typically need.
  waspProjectDir <- findWaspProjectRootDirFromCwd
  compileWithOptions $ defaultCompileOptions waspProjectDir

compileWithOptions :: CompileOptions -> Command ()
compileWithOptions options = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let outDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Compiling wasp code..."
  (warnings, errors) <- liftIO $ compileIOWithOptions options waspProjectDir outDir
  printCompilerWarningsAndStatusAndThrowIfAnyErrors (warnings, errors)

printCompilerWarningsAndStatusAndThrowIfAnyErrors :: ([CompileWarning], [CompileError]) -> Command ()
printCompilerWarningsAndStatusAndThrowIfAnyErrors (warns, errs) = do
  liftIO $ printCompilerWarningsIfAny warns
  if null errs
    then cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."
    else throwError $ CommandError "Compilation failed" $ formatErrorOrWarningMessages errs

printCompilerWarningsIfAny :: [CompileWarning] -> IO ()
printCompilerWarningsIfAny [] = return ()
printCompilerWarningsIfAny warns =
  cliSendMessage $
    Msg.Warning "Your project compiled with warnings" $ formatErrorOrWarningMessages warns ++ "\n\n"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
compileIO waspProjectDir outDir =
  compileIOWithOptions (defaultCompileOptions waspProjectDir) waspProjectDir outDir

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
compileIOWithOptions options waspProjectDir outDir =
  Wasp.Lib.compile waspProjectDir outDir options

formatErrorOrWarningMessages :: [String] -> String
formatErrorOrWarningMessages = intercalate "\n" . map ("- " ++)

defaultCompileOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions
defaultCompileOptions waspProjectDir =
  CompileOptions
    { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir,
      isBuild = False,
      sendMessage = cliSendMessage,
      generatorWarningsFilter = id
    }
