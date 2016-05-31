module Factory where

import Cookhouse.Data.Project

-- A project which the libraries depend on
commonsProject :: Project
commonsProject = Project
  { projectIdentifier     = "commons"
  , projectSource         = Source "darcs" "fake@server.com:/repos/commons"
  , projectDependencies   = []
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildsteps = []
  }

libAProject :: Project
libAProject = Project
  { projectIdentifier     = "libA"
  , projectSource         = Source "darcs" "fake@server.com:/repos/liba"
  , projectDependencies   = ["commons"]
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildsteps = []
  }

libBProject :: Project
libBProject = Project
  { projectIdentifier     = "libB"
  , projectSource         = Source "darcs" "fake@server.com:/repos/libb"
  , projectDependencies   = ["commons"]
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildsteps = []
  }

websiteProject :: Project
websiteProject = Project
  { projectIdentifier     = "website"
  , projectSource         = Source "darcs" "fake@server.com:/repos/website"
  , projectDependencies   = ["libA", "libB", "commons"]
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildsteps = []
  }

independentProject :: Project
independentProject = Project
  { projectIdentifier     = "independent"
  , projectSource         = Source "darcs" "fake@server.com:/repos/independent"
  , projectDependencies   = []
  , projectTriggers       = []
  , projectBuildSteps     = []
  , projectPostBuildsteps = []
  }

websiteWithIndependentProject :: Project
websiteWithIndependentProject = websiteProject
  { projectIdentifier   = "website-with-independent"
  , projectDependencies = ["libA", "libB", "commons", "independent"]
  }
