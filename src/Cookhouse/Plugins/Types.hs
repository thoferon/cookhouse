module Cookhouse.Plugins.Types
  ( PluginM
  , runPlugin
  , PluginConfig
  , SimpleValue(..)
  , lookupConfig
  , lookupConfigBool
  , lookupConfigInt
  , lookupConfigString
  , getConfigBool
  , getConfigInt
  , getConfigString
  -- * Authentication plugins
  , Token(..)
  , AccessLevel(..)
  , AuthenticationPlugin(..)
  , authPluginWithDefaultConfig
  -- * Source plugins
  , SourcePlugin(..)
  , sourcePluginWithDefaultConfig
  -- * Trigger plugins
  , TriggerPlugin(..)
  , triggerPluginWithDefaultConfig
  -- * Step plugins
  , StepPlugin(..)
  , stepPluginWithDefaultConfig
  , emptyRollback
  ) where

import           Control.Applicative
import           Control.Monad.Except

import           Data.Aeson
import qualified Data.Text as T

import           System.IO

import           Servant.API

{-
 - Generic types
 -}

type PluginM = ExceptT String IO

runPlugin :: MonadIO m => PluginM a -> m (Either String a)
runPlugin = liftIO . runExceptT

type PluginConfig = [(String, SimpleValue)]

data SimpleValue
  = SVBool   Bool
  | SVInt    Int
  | SVString String
  deriving (Eq, Show)

instance FromJSON SimpleValue where
  parseJSON v = do
    (SVBool <$> parseJSON v)
    <|> (SVInt <$> parseJSON v)
    <|> (SVString <$> parseJSON v)

lookupConfig :: PluginConfig -> String -> Maybe SimpleValue
lookupConfig = flip lookup

lookupConfigBool :: PluginConfig -> String -> Maybe Bool
lookupConfigBool config key = do
  sv <- lookupConfig config key
  case sv of
    SVBool b -> return b
    _ -> Nothing

lookupConfigInt :: PluginConfig -> String -> Maybe Int
lookupConfigInt config key = do
  sv <- lookupConfig config key
  case sv of
    SVInt i -> return i
    _ -> Nothing

lookupConfigString :: PluginConfig -> String -> Maybe String
lookupConfigString config key = do
  sv <- lookupConfig config key
  case sv of
    SVString s -> return s
    _ -> Nothing

getConfigBool :: PluginConfig -> String -> PluginM Bool
getConfigBool config key = case lookupConfigBool config key of
  Nothing -> throwError $
    "Expected boolean for key " ++ show key ++ " in plugin config"
  Just b -> return b

getConfigInt :: PluginConfig -> String -> PluginM Int
getConfigInt config key = case lookupConfigInt config key of
  Nothing -> throwError $
    "Expected integer for key " ++ show key ++ " in plugin config"
  Just i -> return i

getConfigString :: PluginConfig -> String -> PluginM String
getConfigString config key = case lookupConfigString config key of
  Nothing -> throwError $
    "Expected string for key " ++ show key ++ " in plugin config"
  Just s -> return s

pluginConfigWithDefaults :: PluginConfig -> PluginConfig -> PluginConfig
pluginConfigWithDefaults defs cfg =
  let ks = map fst cfg
  in cfg ++ filter ((`notElem` ks) . fst) defs

{-
 - Authentication plugins
 -}

newtype Token = Token { unToken :: T.Text }

instance ToJSON Token where
  toJSON = toJSON . unToken

instance FromJSON Token where
  parseJSON = fmap Token . parseJSON

instance FromHttpApiData Token where
  parseUrlPiece = Right . Token

data AccessLevel = Admin | User | None

data AuthenticationPlugin = AuthenticationPlugin
  { authPluginName           :: String
  , authPluginTitle          :: String -- ^ Human-readable name of the plugin
  , authPluginSignin         :: String -- ^ Username
                             -> String -- ^ Password
                             -> PluginConfig -- ^ Extra configuration
                             -> PluginM (Maybe Token)
  , authPluginGetAccessLevel :: Token -> PluginConfig -> PluginM AccessLevel
  , authPluginSignout        :: Token -> PluginConfig -> PluginM ()
  }

authPluginWithDefaultConfig :: AuthenticationPlugin -> PluginConfig
                            -> AuthenticationPlugin
authPluginWithDefaultConfig pl@AuthenticationPlugin{..} defs = pl
  { authPluginSignin = \u p ->
      authPluginSignin u p . pluginConfigWithDefaults defs
  , authPluginGetAccessLevel = \t ->
      authPluginGetAccessLevel t . pluginConfigWithDefaults defs
  , authPluginSignout = \t ->
      authPluginSignout t . pluginConfigWithDefaults defs
  }

{-
 - Source plugins
 -}

data SourcePlugin = SourcePlugin
  { sourcePluginName  :: String
  , sourcePluginFetch :: String       -- ^ Field 'location' in the config
                      -> FilePath     -- ^ Local directory to pull the repo to
                      -> PluginConfig -- ^ Extra configuration
                      -> PluginM ()
  , sourcePluginPull  :: String       -- ^ Field 'location' in the config
                      -> FilePath     -- ^ Local directory to pull the repo to
                      -> PluginConfig -- ^ Extra configuration
                      -> PluginM Bool -- ^ Whether there was something to pull
  }

sourcePluginWithDefaultConfig :: SourcePlugin -> PluginConfig -> SourcePlugin
sourcePluginWithDefaultConfig pl@SourcePlugin{..} defs = pl
  { sourcePluginFetch = \l p ->
      sourcePluginFetch l p . pluginConfigWithDefaults defs
  , sourcePluginPull = \l p ->
      sourcePluginPull l p . pluginConfigWithDefaults defs
  }

{-
 - Trigger plugins
 -}

data TriggerPlugin = TriggerPlugin
  { triggerPluginName  :: String
  , triggerPluginCheck :: (FilePath -> PluginM ())
                       -- ^ A function to make the source plugin fetch the
                       -- repository
                       -> (FilePath -> PluginM Bool)
                       -- ^ A function to make the source plugin check for
                       -- changes
                       -> FilePath     -- ^ Directory of the project
                       -> PluginConfig -- ^ Extra configuration
                       -> PluginM Bool
  }

triggerPluginWithDefaultConfig :: TriggerPlugin -> PluginConfig -> TriggerPlugin
triggerPluginWithDefaultConfig pl@TriggerPlugin{..} defs = pl
  { triggerPluginCheck = \f g p ->
      triggerPluginCheck f g p . pluginConfigWithDefaults defs
  }

{-
 - Step plugins
 -}

-- | A plugin to (post-)build the project
data StepPlugin = StepPlugin
  { stepPluginName     :: String
  , stepPluginRun      :: FilePath           -- ^ Directory of the build
                       -> [(String, String)] -- ^ Environment variables
                       -> Handle             -- ^ Handle of the output file
                       -> PluginConfig       -- ^ Extra configuration
                       -> PluginM Bool       -- ^ Whether the step has succeeded
  , stepPluginRollback :: FilePath -> [(String, String)] -> Handle
                       -> PluginConfig -> PluginM Bool
  }

stepPluginWithDefaultConfig :: StepPlugin -> PluginConfig -> StepPlugin
stepPluginWithDefaultConfig pl@StepPlugin{..} defs = pl
  { stepPluginRun = \p e h ->
      stepPluginRun p e h . pluginConfigWithDefaults defs
  , stepPluginRollback = \p e h ->
      stepPluginRollback p e h . pluginConfigWithDefaults defs
  }

emptyRollback :: FilePath -> [(String, String)] -> Handle -> PluginConfig
              -> PluginM Bool
emptyRollback _ _ _ _ = return True
