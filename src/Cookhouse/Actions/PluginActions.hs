module Cookhouse.Actions.PluginActions where

import Cookhouse.Actions.Helpers
import Cookhouse.Plugins.Types

getPluginsAction :: AppSpockAction ()
getPluginsAction = do
  authPluginNames <- map authPluginName <$> getAuthenticationPlugins
  setStatus ok200
  json $ object [ "authentication_plugins" .= authPluginNames ]
