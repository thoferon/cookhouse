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
  authPluginDescrs   <- map AuthPluginDescription <$> getAuthenticationPlugins
  triggerPluginNames <- map triggerPluginName     <$> getTriggerPlugins
  sourcePluginNames  <- map sourcePluginName      <$> getSourcePlugins
  stepPluginNames    <- map stepPluginName        <$> getStepPlugins

  setStatus ok200
  json $ object
    [ "authentication_plugins" .= authPluginDescrs
    , "trigger_plugins"        .= triggerPluginNames
    , "source_plugins"         .= sourcePluginNames
    , "step_plugins"           .= stepPluginNames
    ]
