module Cookhouse.Actions.ProjectActions where

import Cookhouse.Actions.Types
import Cookhouse.Capabilities
import Cookhouse.Data.Project
import Cookhouse.Environment

getProjectsAction :: Action [Project]
getProjectsAction = do
  ensureAccess CAGetProjects
  getProjects
