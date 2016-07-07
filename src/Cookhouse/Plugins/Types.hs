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
  -- * Source plugins
  , SourcePlugin(..)
  -- * Trigger plugins
  , TriggerPlugin(..)
  -- * Step plugins
  , StepPlugin(..)
  , emptyRollback
  ) where

import           Control.Applicative
import           Control.Monad.Except

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS

import           System.IO

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

{-
 - Authentication plugins
 -}

newtype Token = Token { unToken :: BS.ByteString }

instance ToJSON Token where
  toJSON = toJSON . BS.unpack . unToken

data AccessLevel = Admin | User | ReadOnly

data AuthenticationPlugin = AuthenticationPlugin
  { authPluginName           :: String
  , authPluginTitle          :: String -- ^ Human-readable name of the plugin
  , authPluginSignin         :: String -- ^ Username
                             -> String -- ^ Password
                             -> PluginM (Maybe Token)
  , authPluginGetAccessLevel :: Token -> PluginM AccessLevel
  , authPluginSignout        :: Token -> PluginM ()
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

{-
 - Step plugins
 -}

-- | A plugin to (post-)build the project
data StepPlugin = StepPlugin
  { stepPluginName     :: String
  , stepPluginRun      :: FilePath     -- ^ Directory of the build
                       -> Handle       -- ^ Handle of the output file
                       -> PluginConfig -- ^ Extra configuration
                       -> PluginM Bool -- ^ Whether the step has succeeded
  , stepPluginRollback :: FilePath -> Handle -> PluginConfig -> PluginM Bool
  }

emptyRollback :: FilePath -> Handle -> PluginConfig -> PluginM Bool
emptyRollback _ _ _ = return True
