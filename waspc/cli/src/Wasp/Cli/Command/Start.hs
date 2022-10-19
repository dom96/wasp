module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent (readMVar, swapMVar)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newMVar)
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
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."
  compilationResult <- liftIO $ compileIO waspRoot outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError "Compilation failed" compileError
    Right () -> cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."
  watchOrStartResult <-
    liftIO $ do
      isRestartInProgressMVar <- newMVar False
      watch waspRoot outDir isRestartInProgressMVar
        -- TODO: On jobs quiet down, print Wasp warnings and errors.
        --   But, do it only once after the restart caused by watch!
        --   Maybe I could give a channel to `watch`, to which it would write
        --   on every restart, and then the handler I pass here would be listening
        --   to that channel and when it gets triggered, it would do something only
        --   if the channel is not empty. Not sure how I can learn that since it will
        --   block on the channel though. Maybe I should use some kind of mvar instead.
        --   The point is: write warns/errs only if you haven't written them since the last
        --   restart triggered by watch. We do this to avoid writing errors again and again
        --   in case there is for example some console.log output coming from the server.
        `race` ( Wasp.Lib.start
                   outDir
                   ( do
                       isRestartInProgress <- readMVar isRestartInProgressMVar
                       when isRestartInProgress $ do
                         _ <- swapMVar isRestartInProgressMVar False
                         putStrLn "TODO: Print warnings and error messages."
                   )
               )
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."
