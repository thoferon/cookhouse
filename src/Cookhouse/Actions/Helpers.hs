{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Actions.Helpers
  ( inDataLayer
  , getToken
  , getAuthenticationPluginName
  , failAction
  , withDataLayerResult
  , POF
  , runPOF
  , paramPOF
  , paramPOFMaybe
  , listPOF
  , listPOFMaybe
  , pofError
  , bfnToMaybe
  , withProject
  , module Control.Applicative
  , module Data.Aeson
  , module Network.HTTP.Types.Status
  , module Web.Spock.Simple
  , module Cookhouse.Actions.Types
  , module Cookhouse.Environment
  ) where

import           Control.Applicative
import           Control.Monad.Except

import           Data.Aeson hiding (json)
import           Data.List
import           Data.Monoid
import           Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           System.IO

import           Database.PostgreSQL.Simple

import           Network.HTTP.Types.Status

import           Web.PathPieces
import           Web.Spock.Simple hiding (head)

import           Cookhouse.Actions.Types
import           Cookhouse.Capabilities
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types
import           Cookhouse.Environment
import           Cookhouse.Errors
import           Cookhouse.Plugins.Types

inDataLayer :: DataM a -> AppSpockAction (Either CookhouseError a)
inDataLayer action = do
  mToken      <- getToken
  mPluginName <- getAuthenticationPluginName

  eCapabilities <- case (mToken, mPluginName) of
    (Nothing, _) -> return $ Right [anonymousCapability]
    (_, Nothing) -> return $ Right [anonymousCapability]
    (Just token, Just name) -> do
      mPlugin <- findAuthenticationPlugin name
      case mPlugin of
        Nothing -> return $ Right [anonymousCapability]
        Just plugin -> liftIO $ runExceptT $
          withExceptT (AuthenticationPluginError name) $ do
            level <- authPluginGetAccessLevel plugin token
            return [toCookhouseCapability level]

  now <- liftIO getCurrentTime
  case eCapabilities of
    Left  err          -> return $ Left err
    Right capabilities -> runQuery $ \conn -> do
      eRes <- runDatabaseM conn $ runTimeT now $
        runExceptT $ runSafeAccessT action capabilities
      return $ case eRes of
        Left  err                   -> Left err
        Right (Left  err)           -> Left err
        Right (Right (Left  descr)) -> Left $ PermissionError descr
        Right (Right (Right res))   -> Right res

getToken :: AppSpockAction (Maybe Token)
getToken = fmap (Token . TE.encodeUtf8) <$> header "X-API-Token"

getAuthenticationPluginName :: AppSpockAction (Maybe String)
getAuthenticationPluginName =
  fmap T.unpack <$> header "X-API-Authentication-Plugin"

newtype ApiError = ApiError String

instance ToJSON ApiError where
  toJSON (ApiError msg) = object [ "error" .= msg ]

failAction :: HTTPError e => e -> AppSpockAction ()
failAction err = do
  liftIO $ hPutStrLn stderr $ "Error: " ++ show err
  let (status, msg) = errStatusAndMsg err
  setStatus status
  json $ ApiError msg

withDataLayerResult :: Either CookhouseError a -> (a -> AppSpockAction ())
                    -> AppSpockAction ()
withDataLayerResult eRes f = case eRes of
  Left  err -> failAction err
  Right res -> f res

-- Can't use the type synonym 'AppSpockAction' here...
newtype POF a
  = POF { unPOF :: ExceptT String (ActionT
                                   (WebStateM Connection () Environment)) a }
  deriving (Functor, Applicative, Monad)

runPOF :: POF a -> (a -> AppSpockAction ()) -> AppSpockAction ()
runPOF pof f = do
  eRes <- runExceptT $ unPOF pof
  case eRes of
    Right res -> f res
    Left  err -> failAction $ ParamError err

paramPOF :: PathPiece p => T.Text -> POF p
paramPOF name = do
  mRes <- paramPOFMaybe name
  case mRes of
    Just res -> return res
    Nothing  -> POF $ throwError $ "Parameter \""
                                ++ T.unpack name
                                ++ "\" is missing or malformed."

paramPOFMaybe :: PathPiece p => T.Text -> POF (Maybe p)
paramPOFMaybe = POF . lift . param

listPOF :: PathPiece p => T.Text -> POF [p]
listPOF name = do
    count <- paramPOF $ name <> "_count"
    go count []
  where
    go :: PathPiece p => Integer -> [p] -> POF [p]
    go 0 acc = return acc
    go n acc = do
      x <- paramPOF $ name <> T.pack (show (n - 1))
      go (n - 1) $ x : acc

listPOFMaybe :: PathPiece p => T.Text -> POF (Maybe [p])
listPOFMaybe name = do
  mCount <- paramPOFMaybe $ name <> "_count" :: POF (Maybe Integer)
  case mCount of
    Nothing -> return Nothing
    Just _  -> Just <$> listPOF name

pofError :: String -> POF a
pofError = POF . throwError

data BlankForNothing a = Blank | Something a

instance PathPiece a => PathPiece (BlankForNothing a) where
  fromPathPiece txt | T.null txt = Just Blank
                    | otherwise  = Something <$> fromPathPiece txt
  toPathPiece = maybe "" toPathPiece . bfnToMaybe

bfnToMaybe :: BlankForNothing a -> Maybe a
bfnToMaybe bnf = case bnf of
  Blank       -> Nothing
  Something a -> Just a

instance PathPiece UTCTime where
  fromPathPiece =
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . T.unpack
  toPathPiece   = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

withProject :: ProjectIdentifier -> (Project -> AppSpockAction ())
            -> AppSpockAction ()
withProject identifier cont = do
  projects <- getProjects
  case find ((== identifier) . projectIdentifier) projects of
    Nothing -> failAction $
      IncorrectProjectIdentifierError $ unProjectIdentifier identifier
    Just project -> cont project
