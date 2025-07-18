{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.CmdRun.DS.Utility where

import System.Log.FastLogger
import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.Exit
import Control.Lens

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import PMS.Infra.CmdRun.DM.Type

-- |
--
runApp :: DM.DomainData -> AppData -> TimedFastLogger -> AppContext a -> IO (Either DM.ErrorData a)
runApp domDat appDat logger ctx =
  DM.runFastLoggerT domDat logger
    $ runExceptT
    $ flip runReaderT domDat
    $ runReaderT ctx appDat


-- |
--
liftIOE :: IO a -> AppContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go x = E.catchAny (Right <$> x) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show

{-
-- |
--
validateCommand :: String -> AppContext String
validateCommand cmd = do
  when (null cmd) $
    throwError "Command is empty."

  when (".." `T.isInfixOf` tcmd) $
    throwError "Command contains directory traversal '..'."

  when ("/" `T.isInfixOf` tcmd) $
    throwError "Command must not contain '/'."

  when ("\\" `T.isInfixOf` tcmd) $
    throwError "Command must not contain '\\'."

  when (any (not . isAllowedChar) cmd) $
    throwError $ "Command contains disallowed characters: " ++ cmd

  return cmd

  where
    tcmd = T.pack cmd
    isAllowedChar c = isAlphaNum c || c `elem` ("-._" :: String)
  

-- |
--
validateCommandArg :: String -> AppContext String
validateCommandArg arg = do
  let tArg = T.pack arg
  when (hasDangerousChars tArg) $
    throwError $ "Argument contains potentially dangerous characters: " <> arg
  return arg
  where
    hasDangerousChars :: T.Text -> Bool
    hasDangerousChars txt =
      any (`T.isInfixOf` txt) [";", "&&", "|", "$", "`", "<", ">", "\\", "\""]

-- |
--
validateCommandArgs :: [String] -> AppContext [String]
validateCommandArgs = mapM validateCommandArg


-- |
--
validateMessage :: String -> IO String
validateMessage cmd = do
  when (any (`elem` forbiddenChars) cmd) $
    E.throwString "Command contains forbidden characters."

  case words cmd of
    (firstWord : _) -> when (firstWord `elem` forbiddenCommands) $
                        E.throwString "Command is forbidden."
    _ -> return ()

  return cmd
  where
    forbiddenChars :: [Char]
    forbiddenChars = [';', '&', '|', '`']

    forbiddenCommands :: [String]
    forbiddenCommands = ["rm", "mv", "dd", "chmod", "chown", "shutdown", "reboot", "kill", "nc", "telnet", "ssh"]
-}

-- |
--
toolsCallResponse :: STM.TQueue DM.McpResponse
                  -> DM.JsonRpcRequest
                  -> ExitCode
                  -> String
                  -> String
                  -> IO ()
toolsCallResponse resQ jsonRpc code outStr errStr = do
  let content = [ DM.McpToolsCallResponseResultContent "text" outStr
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
errorToolsCallResponse :: DM.JsonRpcRequest -> String -> AppContext ()
errorToolsCallResponse jsonRpc errStr = do
  let content = [ DM.McpToolsCallResponseResultContent "text" errStr ]
      result = DM.McpToolsCallResponseResult {
                  DM._contentMcpToolsCallResponseResult = content
                , DM._isErrorMcpToolsCallResponseResult = True
                }
      resDat = DM.McpToolsCallResponseData jsonRpc result
      res = DM.McpToolsCallResponse resDat

  resQ <- view DM.responseQueueDomainData <$> lift ask
  liftIOE $ STM.atomically $ STM.writeTQueue resQ res

