module Wasp.Cli.Command.Watch
  ( watch,
  )
where

import Control.Concurrent (MVar, swapMVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (tryPutMVar, tryTakeMVar)
import Control.Monad (unless)
import Data.List (isSuffixOf)
import Data.Time.Clock (UTCTime, getCurrentTime)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import qualified System.FSNotify as FSN
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (compileIO)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Lib (CompileError, CompileWarning)
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

-- TODO: Idea: Read .gitignore file, and ignore everything from it. This will then also cover the
--   .wasp dir, and users can easily add any custom stuff they want ignored. But, we also have to
--   be ready for the case when there is no .gitignore, that could be possible.

-- | Forever listens for any file changes in waspProjectDir, and if there is a change,
--   compiles Wasp source files in waspProjectDir and regenerates files in outDir.
--   It will defer recompilation until no new change was detected in the last second.
-- TODO: Document what isRestartInProgressMVar does / is.
--   What it is: If True, it means underlying services are potentially restarting the client and/or server.
watch ::
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  MVar ([CompileWarning], [CompileError]) ->
  IO ()
watch waspProjectDir outDir ongoingCompilationResultMVar = FSN.withManager $ \mgr -> do
  currentTime <- getCurrentTime
  chan <- newChan
  _ <-
    FSN.watchDirChan
      mgr
      (SP.fromAbsDir waspProjectDir)
      eventFilter
      chan
  _ <-
    FSN.watchTreeChan
      mgr
      (SP.fromAbsDir $ waspProjectDir </> Common.extCodeDirInWaspProjectDir)
      eventFilter
      chan
  listenForEvents chan currentTime
  where
    listenForEvents :: Chan FSN.Event -> UTCTime -> IO ()
    listenForEvents chan lastCompileTime = do
      event <- readChan chan
      if isStaleEvent event lastCompileTime
        then -- Ignore delayed/stale events older than our last compile time.
          listenForEvents chan lastCompileTime
        else do
          -- Recompile, but only after a 1s period of no new events.
          waitUntilNoNewEvents chan lastCompileTime 1
          currentTime <- getCurrentTime
          recompile
          -- TODO: Below, set mvar to output from recompile -> warnings and errors.
          _ <- tryTakeMVar ongoingCompilationResultMVar >> tryPutMVar ongoingCompilationResultMVar ([], [])
          listenForEvents chan currentTime

    -- Blocks until no new events are recieved for a duration of `secondsToDelay`.
    -- Consumes any new events during an active timer window and then restarts wait.
    -- If a stale event comes in during an active timer window, we immediately
    -- return control to the caller.
    waitUntilNoNewEvents :: Chan FSN.Event -> UTCTime -> Int -> IO ()
    waitUntilNoNewEvents chan lastCompileTime secondsToDelay = do
      eventOrDelay <- race (readChan chan) (threadDelaySeconds secondsToDelay)
      case eventOrDelay of
        Left event -> do
          unless (isStaleEvent event lastCompileTime) $
            -- We have a new event, restart waiting process.
            waitUntilNoNewEvents chan lastCompileTime secondsToDelay
        Right () -> return ()

    isStaleEvent :: FSN.Event -> UTCTime -> Bool
    isStaleEvent event lastCompileTime = FSN.eventTime event < lastCompileTime
    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    recompile :: IO ()
    recompile = do
      cliSendMessage $ Msg.Start "Recompiling on file change..."
      (warnings, errors) <- compileIO waspProjectDir outDir
      printCompilerWarningsIfAny warnings
      if null errors
        then
          cliSendMessage $
            Msg.Success "Recompilation on file change succeeded."
        else -- TODO: duplication of output logic?

          cliSendMessage $
            Msg.Failure "Recompilation on file change failed" $
              formatErrorOrWarningMessages errors

    -- TODO: This is a hardcoded approach to ignoring most of the common tmp files that editors
    --   create next to the source code. Bad thing here is that users can't modify this,
    --   so better approach would be probably to use information from .gitignore instead, or
    --   maybe combining the two somehow.
    eventFilter :: FSN.Event -> Bool
    eventFilter event =
      let filename = FP.takeFileName $ FSN.eventPath event
       in not (null filename)
            && take 2 filename /= ".#" -- Ignore emacs lock files.
            && not (head filename == '#' && last filename == '#') -- Ignore emacs auto-save files.
            && last filename /= '~' -- Ignore emacs and vim backup files.
            && not (head filename == '.' && ".swp" `isSuffixOf` filename) -- Ignore vim swp files.
            && not (head filename == '.' && ".un~" `isSuffixOf` filename) -- Ignore vim undo files.
