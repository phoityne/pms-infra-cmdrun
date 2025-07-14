{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module PMS.Infra.CmdRun.DS.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Data.Conduit
import qualified Control.Concurrent as CC
import Control.Concurrent.Async
import qualified Data.Text as T
import Control.Monad.Except
import System.Process
import System.FilePath
import Data.Aeson
import qualified Control.Exception.Safe as E
import System.Exit

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.CmdRun.DM.Type
import PMS.Infra.CmdRun.DS.Utility


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| cmd2task .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.CmdRunCommand AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.CmdRunCommand
    go = do
      queue <- view DM.cmdRunQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
cmd2task :: ConduitT DM.CmdRunCommand (IOTask ()) AppContext ()
cmd2task = await >>= \case
  Just cmd -> flip catchError (errHdl cmd) $ do
    lift (go cmd) >>= yield >> cmd2task
  Nothing -> do
    $logWarnS DM._LOGTAG "cmd2task: await returns nothing. skip."
    cmd2task

  where
    errHdl :: DM.CmdRunCommand -> String -> ConduitT DM.CmdRunCommand (IOTask ()) AppContext ()
    errHdl cmdCmd msg = do
      let jsonrpc = DM.getJsonRpcCmdRunCommand cmdCmd
      $logWarnS DM._LOGTAG $ T.pack $ "cmd2task: exception occurred. skip. " ++ msg
      lift $ errorToolsCallResponse jsonrpc $ "cmd2task: exception occurred. skip. " ++ msg
      cmd2task

    go :: DM.CmdRunCommand -> AppContext (IOTask ())
    go (DM.EchoCmdRunCommand dat) = genEchoTask dat
    go (DM.DefaultCmdRunCommand dat) = genCmdRunTask dat

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT (IOTask ()) Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT (IOTask ()) Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink

    go :: (IO ()) -> AppContext ()
    go t = do
      $logDebugS DM._LOGTAG "sink: start async."
      _ <- liftIOE $ async t
      $logDebugS DM._LOGTAG "sink: end async."
      return ()

---------------------------------------------------------------------------------
-- |
--
genEchoTask :: DM.EchoCmdRunCommandData -> AppContext (IOTask ())
genEchoTask dat = do
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let val = dat^.DM.valueEchoCmdRunCommandData

  $logDebugS DM._LOGTAG $ T.pack $ "echoTask: echo : " ++ val
  return $ echoTask resQ dat val


-- |
--   
echoTask :: STM.TQueue DM.McpResponse -> DM.EchoCmdRunCommandData -> String -> IOTask ()
echoTask resQ cmdDat val = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.CmdRun.DS.Core.work.echoTask run. " ++ val

  response ExitSuccess val ""

  hPutStrLn stderr "[INFO] PMS.Infra.CmdRun.DS.Core.work.echoTask end."

  where
    errHdl :: E.SomeException -> IO ()
    errHdl e = response (ExitFailure 1) "" (show e)

    response :: ExitCode -> String -> String -> IO ()
    response code outStr errStr = do
      let jsonRpc = cmdDat^.DM.jsonrpcEchoCmdRunCommandData
          content = [ DM.McpToolsCallResponseResultContent "text" outStr
                    , DM.McpToolsCallResponseResultContent "text" errStr
                    ]
          result = DM.McpToolsCallResponseResult {
                      DM._contentMcpToolsCallResponseResult = content
                    , DM._isErrorMcpToolsCallResponseResult = (ExitSuccess /= code)
                    }
          resDat = DM.McpToolsCallResponseData jsonRpc result
          res = DM.McpToolsCallResponse resDat

      STM.atomically $ STM.writeTQueue resQ res

-- |
--
genCmdRunTask :: DM.DefaultCmdRunCommandData -> AppContext (IOTask ())
genCmdRunTask dat = do
  toolsDir <- view DM.toolsDirDomainData <$> lift ask
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let nameTmp = dat^.DM.nameDefaultCmdRunCommandData
      argsBS = DM.unRawJsonByteString $ dat^.DM.argumentsDefaultCmdRunCommandData
  args <- liftEither $ eitherDecode $ argsBS
  
  name <- liftIOE $ DM.validateCommand nameTmp
  argsStr <- liftIOE $ DM.validateArg $ args^.argumentsStringToolParams
#ifdef mingw32_HOST_OS
  let scriptExt = ".bat"
#else
  let scriptExt = ".sh"
#endif

  let cmd = toolsDir </> name ++ scriptExt ++ " " ++ argsStr

  $logDebugS DM._LOGTAG $ T.pack $ "cmdRunTask: system cmd. " ++ cmd
  return $ cmdRunTask resQ dat cmd


-- |
--   
cmdRunTask :: STM.TQueue DM.McpResponse -> DM.DefaultCmdRunCommandData -> String -> IOTask ()
cmdRunTask resQ cmdDat cmd = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.CmdRun.DS.Core.work.cmdRunTask run. " ++ cmd
  let tout = 30 * 1000 * 1000

  race (readCreateProcessWithExitCode (shell cmd) "") (CC.threadDelay tout) >>= \case
    Left (code, out, err)  -> response code out err
    Right _ -> E.throwString "timeout occurred."

  hPutStrLn stderr "[INFO] PMS.Infra.CmdRun.DS.Core.work.cmdRunTask end."

  where
    errHdl :: E.SomeException -> IO ()
    errHdl e = response (ExitFailure 1) "" (show e)

    response :: ExitCode -> String -> String -> IO ()
    response code outStr errStr = do
      let jsonRpc = cmdDat^.DM.jsonrpcDefaultCmdRunCommandData
          content = [ DM.McpToolsCallResponseResultContent "text" outStr
                    , DM.McpToolsCallResponseResultContent "text" errStr
                    ]
          result = DM.McpToolsCallResponseResult {
                      DM._contentMcpToolsCallResponseResult = content
                    , DM._isErrorMcpToolsCallResponseResult = (ExitSuccess /= code)
                    }
          resDat = DM.McpToolsCallResponseData jsonRpc result
          res = DM.McpToolsCallResponse resDat

      STM.atomically $ STM.writeTQueue resQ res
