module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent (Chan, dupChan, forkIO, newChan, readChan, threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import Data.Time (getCurrentTime)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory (copyFile, createDirectoryIfMissing, renamePath)
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
import qualified Wasp.Data
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Generator.Job as Job
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  compileChannel <- liftIO newChan -- TODO: pass into compileIO and watch
  compilationResult <- liftIO $ compileIO waspRoot outDir (Just compileChannel)
  case compilationResult of
    Left compileError -> throwError $ CommandError "Compilation failed" compileError
    Right () -> cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."

  startChannel <- liftIO newChan
  liftIO $ dupChan startChannel >>= writePowerlineFiles outDir compileChannel
  watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Wasp.Lib.start outDir startChannel)
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."

data PowerlineOutputDir

powerlineOutputDir :: Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir PowerlineOutputDir)
powerlineOutputDir generatedCodeDir = generatedCodeDir </> [SP.reldir|powerline|]

-- TODO:
-- Need to handle Wasp messages.
-- Figure out where to write and how to automatically open.
-- Have separate areas for warning/errors that we have seen.
-- Can show some sort of app structure diagram, etc. (May require AppSpec)
writePowerlineFiles :: Path' Abs (Dir ProjectRootDir) -> Chan String -> Chan JobMessage -> IO ()
writePowerlineFiles generatedCodeDir compileChannel startChannel = do
  dataDir <- Wasp.Data.getAbsDataDirPath
  let powerlineOutDir = powerlineOutputDir generatedCodeDir

  createDirectoryIfMissing False (SP.fromAbsDir powerlineOutDir)

  copyFile
    (SP.fromAbsFile (dataDir </> [SP.relfile|Cli/templates/powerline/powerline.css|]))
    (SP.fromAbsFile (powerlineOutDir </> [SP.relfile|powerline.css|]))

  _ <- forkIO $ writeOutput powerlineOutDir compileChannel startChannel ([], [])
  return ()

writeOutput :: Path' Abs (Dir PowerlineOutputDir) -> Chan String -> Chan JobMessage -> ([String], [JobMessage]) -> IO ()
writeOutput powerlineOutDir compileChannel startChannel (compileMessages, jobMessages) = do
  messageOrDelay <- race (race (readChan compileChannel) (readChan startChannel)) (threadDelaySeconds 3)
  messages <-
    case messageOrDelay of
      Left eitherJobMessage ->
        case eitherJobMessage of
          Left compileMessage -> return (compileMessage : compileMessages, jobMessages)
          Right jobMessage -> return (compileMessages, jobMessage : jobMessages)
      Right _timerExpired -> return (compileMessages, jobMessages)
  writeOutputFile messages
  writeOutput powerlineOutDir compileChannel startChannel messages
  where
    tmpHtmlOutputFileFp = SP.fromAbsFile $ powerlineOutDir </> [SP.relfile|test.html.tmp|]
    htmlOutputFileFp = SP.fromAbsFile $ powerlineOutDir </> [SP.relfile|test.html|]

    writeOutputFile :: ([String], [JobMessage]) -> IO ()
    writeOutputFile messages = do
      timestamp <- getCurrentTime
      Data.Text.IO.writeFile tmpHtmlOutputFileFp (htmlShell (show timestamp) messages)
      renamePath tmpHtmlOutputFileFp htmlOutputFileFp

    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

-- TODO: write compile output messages too
htmlShell :: String -> ([String], [JobMessage]) -> Text
htmlShell timestamp (waspMessage, jobMessages) =
  Text.unwords
    [ "<html><head>",
      "<title>Wasp Powerline</title>",

      "<script>let shouldRefresh = true;</script>",
      "<script>function disableRefresh() { shouldRefresh = false; document.getElementById('refreshButton').style.display = 'none';  }</script>",
      "<link rel=\"stylesheet\" href=\"./powerline.css\">",

      "</head>",
      "<body>",
      "<div><p>Last write timestamp: " <> pack timestamp <> "</p></div>",
      "<div><p>Last JS refresh timestamp: <span id='jsTime'></span><button id='refreshButton' onclick='disableRefresh();'>Disable Refresh</button></p></div>",
      "<div class='logContainer'>" <> splitJobMessages <> "</div>",
      Text.pack $ "<div class='logContainer'>" ++ "<div class='logSection waspMessages'><h2>Wasp</h2><div class='scrollable'>" ++ intercalate "<br>" (reverse waspMessage) ++ "</div></div></div>",
      "<script>setTimeout(() => { shouldRefresh && location.reload() }, 3000)</script>",
      "<script>document.getElementById('jsTime').innerHTML = new Date();</script>",
      "</body>",
      "</html>"
    ]
  where
    splitJobMessages :: Text
    splitJobMessages =
      let webAppMessages = filter (\jm -> Job._jobType jm == Job.WebApp) jobMessages
          serverMessages = filter (\jm -> Job._jobType jm == Job.Server) jobMessages
       in makeMessagesPretty webAppMessages "Web"
            <> makeMessagesPretty serverMessages "Server"

    makeMessagesPretty :: [JobMessage] -> Text -> Text
    makeMessagesPretty jms title =
      Text.unwords
        [ "<div class='logSection'>",
          "<h2>" <> title <> "</h2>",
          "<div class='scrollable'>",
          Text.intercalate "\n" $ map makeMessagePretty jms,
          "</div>",
          "</div>"
        ]

    -- TODO: Hacky, fix this cleanup some
    makeMessagePretty :: JobMessage -> Text
    makeMessagePretty jm =
      case Job._data jm of
        Job.JobOutput txt _ -> "<div>" <> (Text.intercalate "<br/>" . Text.split (== '\n') . Text.strip . stripAnsiEscapeCodes $ txt) <> "</div>"
        Job.JobExit _ -> ""
