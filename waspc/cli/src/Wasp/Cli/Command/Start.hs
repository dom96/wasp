module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Compile
  ( compileIO,
    printCompilerWarningsAndStatusAndThrowIfAnyErrors,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."
  warningsAndErrors <- liftIO $ compileIO waspRoot outDir
  printCompilerWarningsAndStatusAndThrowIfAnyErrors warningsAndErrors

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."
  watchOrStartResult <-
    liftIO $ do
      ongoingCompilationResultMVar <- newMVar ([], [])
      watch waspRoot outDir ongoingCompilationResultMVar
        `race` Wasp.Lib.start outDir (onJobsQuietDown ongoingCompilationResultMVar)
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."
  where
    onJobsQuietDown :: MVar ([CompileWarning], [CompileError]) -> IO ()
    onJobsQuietDown ongoingCompilationResultMVar = do
      maybeOngoingCompilationResult <- tryTakeMVar ongoingCompilationResultMVar
      case maybeOngoingCompilationResult of
        Nothing -> return ()
        Just (warnings, errors) -> do
          -- TODO: The way we print results of compilation -> warnings, errors -> is very incosistent.
          --   We do it in multiple places:
          --     1. If compile Command fails, warnings are printed but error is thrown and then
          --        caught and printed by Wasp.
          --     2. In Watch, we print both warnings and errors and don't fail on errors.
          --     3. And I think we do this logic somewhere else also.
          --  We should have one logic for printing warnings, and one for printing errors, and then
          --  use them consistently in all these use cases.
          printCompilerWarningsIfAny warnings
          cliSendMessage $ Msg.Failure "Compilation errors" $ formatErrorOrWarningMessages errors ++ "\n\n"
