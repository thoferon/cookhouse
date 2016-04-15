module Cookhouse.Actions.ProjectActions where

import Cookhouse.Actions.Helpers
import Cookhouse.Capabilities

getProjectsAction :: AppSpockAction ()
getProjectsAction = do
  projects <- getProjects
  eRes     <- inDataLayer $ ensureAccess CAGetProjects
  withDataLayerResult eRes $ \_ -> do
    setStatus ok200
    json $ object [ "projects" .= projects ]
