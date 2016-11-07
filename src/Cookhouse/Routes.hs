module Cookhouse.Routes where

import qualified Web.Spock.Simple as S

import           Cookhouse.Actions.Types
import           Cookhouse.Config
import           Cookhouse.Middlewares.CORS

import           Cookhouse.Actions.JobActions
import           Cookhouse.Actions.JobResultActions
import           Cookhouse.Actions.PluginActions
import           Cookhouse.Actions.ProjectActions
import           Cookhouse.Actions.SessionActions

routes :: Config -> AppSpockM ()
routes config = do
  let get    route action = S.get    route $ setCorsHeaders config >> action
      post   route action = S.post   route $ setCorsHeaders config >> action
      delete route action = S.delete route $ setCorsHeaders config >> action

  post "/signin"  signinAction
  post "/signout" signoutAction

  get "/plugins"  getPluginsAction
  get "/projects" getProjectsAction

  get    "/pending_jobs"                       getPendingJobsAction
  get    "/projects/:project_identifier/jobs"  getJobsOfProjectAction
  post   "/projects/:project_identifier/build" generateJobsAction

  get    "/jobs/:job_id" getJobAction
  delete "/jobs/:job_id" deleteJobAction

  get "/job_results/:job_result_id/output" getJobResultOutputAction
