module Cookhouse.Actions.PluginActions where

import Cookhouse.Actions.Types
import Cookhouse.Environment
import Cookhouse.Plugins.Types

data Plugins
  = Plugins [AuthenticationPlugin] [TriggerPlugin] [SourcePlugin] [StepPlugin]

instance ToJSON Plugins where
  toJSON (Plugins authPlugins triggerPlugins sourcePlugins stepPlugins) =
    let authDescrs = map (\AuthenticationPlugin{..} -> object
                          ["name" .= authPluginName, "title" .= authPluginTitle]
                         ) authPlugins
    in object [ "authentication_plugins" .= authDescrs
              , "trigger_plugins"        .= map triggerPluginName triggerPlugins
              , "source_plugins"         .= map sourcePluginName  sourcePlugins
              , "step_plugins"           .= map stepPluginName    stepPlugins
              ]

getPluginsAction :: Action Plugins
getPluginsAction = Plugins
  <$> getAuthenticationPlugins
  <*> getTriggerPlugins
  <*> getSourcePlugins
  <*> getStepPlugins
