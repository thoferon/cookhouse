module Factory where

import Cookhouse.Data.Project

-- A project which the libraries depend on
commonsProject :: Project
commonsProject = Project
  { projectIdentifier     = "commons"
  , projectSource         = Source "darcs" "fake@server.com:/repos/commons" []
  , projectDependencies   = []
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildSteps = []
  , projectArtefacts      = []
  }

libAProject :: Project
libAProject = Project
  { projectIdentifier     = "libA"
  , projectSource         = Source "darcs" "fake@server.com:/repos/liba" []
  , projectDependencies   = ["commons"]
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildSteps = []
  , projectArtefacts      = []
  }

libBProject :: Project
libBProject = Project
  { projectIdentifier     = "libB"
  , projectSource         = Source "darcs" "fake@server.com:/repos/libb" []
  , projectDependencies   = ["commons"]
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildSteps = []
  , projectArtefacts      = []
  }

websiteProject :: Project
websiteProject = Project
  { projectIdentifier     = "website"
  , projectSource         = Source "darcs" "fake@server.com:/repos/website" []
  , projectDependencies   = ["libA", "libB", "commons"]
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildSteps = []
  , projectArtefacts      = []
  }

independentProject :: Project
independentProject = Project
  { projectIdentifier     = "independent"
  , projectSource         = Source "darcs" "fake@server.com:/repos/indep" []
  , projectDependencies   = []
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildSteps = []
  , projectArtefacts      = []
  }

websiteWithIndependentProject :: Project
websiteWithIndependentProject = websiteProject
  { projectIdentifier   = "website-with-independent"
  , projectDependencies = ["libA", "libB", "commons", "independent"]
  }
