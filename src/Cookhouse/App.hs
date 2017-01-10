module Cookhouse.App where

import qualified Data.Text as T

import           Servant

import           Cookhouse.Actions.JobActions
import           Cookhouse.Actions.JobResultActions
import           Cookhouse.Actions.PluginActions
import           Cookhouse.Actions.ProjectActions
import           Cookhouse.Actions.SessionActions
import           Cookhouse.Actions.Types
import           Cookhouse.Data.Job
import           Cookhouse.Data.JobResult
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types
import           Cookhouse.Environment
import           Cookhouse.Middlewares.CORS

type SubCookhouseAPI =
  "signin" :> ReqBody '[JSON] Credentials :> Post '[JSON] Token
  :<|> "signout" :> ReqBody '[JSON] AuthInfo :> PostNoContent '[JSON] NoContent

  :<|> "plugins"  :> Get '[JSON] Plugins
  :<|> "projects" :> Get '[JSON] [Project]

  :<|> "jobs" :> Capture "job_id" (EntityID Job) :> Get '[JSON] JobInfo
  :<|> "jobs" :> Capture "job_id" (EntityID Job) :> Delete '[JSON] NoContent
  :<|> "jobs" :> "pending" :> Get '[JSON] [Entity Job]
  :<|> "projects" :> Capture "project_id" ProjectIdentifier :> "jobs"
                  :> Get '[JSON] [Entity Job]
  :<|> "projects" :> Capture "project_id" ProjectIdentifier :> "build"
                  :> PostCreated '[JSON] [Entity Job]

  :<|> "job_results" :> Capture "job_result_id" (EntityID JobResult)
                     :> QueryParam "offset" Integer :> Get '[PlainText] T.Text

type CookhouseAPI =
  Header "X-API-Token" Token :> Header "X-API-Authentication-Plugin" String
  :> SubCookhouseAPI

subserver :: ServerT SubCookhouseAPI Action
subserver =
  signinAction
  :<|> signoutAction

  :<|> getPluginsAction
  :<|> getProjectsAction

  :<|> getJobAction
  :<|> deleteJobAction
  :<|> getPendingJobsAction
  :<|> getJobsOfProjectAction
  :<|> generateJobsAction

  :<|> getJobResultOutputAction

server :: ServerT CookhouseAPI SubAction
server mToken mName = enter (Nat (actionToSubAction mToken mName)) subserver

app :: Environment -> Application
app env = corsMiddleware (envConfig env) $
  serve (Proxy :: Proxy CookhouseAPI)
        (enter (Nat (subActionToHandler env)) server)
