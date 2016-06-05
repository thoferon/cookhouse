module Cookhouse.Actions.PluginActions where

import Data.Aeson hiding (json)

import Cookhouse.Actions.Helpers
import Cookhouse.Plugins.Types

newtype AuthPluginDescription = AuthPluginDescription AuthenticationPlugin

instance ToJSON AuthPluginDescription where
  toJSON (AuthPluginDescription (AuthenticationPlugin{..})) = object
    ["name" .= authPluginName, "title" .= authPluginTitle]

getPluginsAction :: AppSpockAction ()
getPluginsAction = do
  authPluginDescrs <- map AuthPluginDescription <$> getAuthenticationPlugins
  setStatus ok200
  json $ object [ "authentication_plugins" .= authPluginDescrs ]
