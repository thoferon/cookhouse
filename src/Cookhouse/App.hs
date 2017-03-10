{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.App where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import           Network.HTTP.Media ((//))

import           Servant
import           Servant.API.ContentTypes

import           Cookhouse.Actions.ArtefactActions
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

data Artefact

instance AllCTRender '[Artefact] BSL.ByteString where
  handleAcceptH _ (AcceptHeader h) v = Just (BSL.fromStrict h, v)

instance AllCTUnrender '[Artefact] BSL.ByteString where
  handleCTypeH _ h v = Just (Right v)

instance Accept Artefact where
  contentType _ = "text" // "any"

type SubCookhouseAPI =
  "signin" :> ReqBody '[FormUrlEncoded] Credentials :> Post '[JSON] Token
  :<|> "signout" :> ReqBody '[FormUrlEncoded] AuthInfo
                 :> PostNoContent '[JSON] NoContent

  :<|> "plugins"  :> Get '[JSON] Plugins
  :<|> "projects" :> Get '[JSON] [Project]

  :<|> "jobs" :> Capture "job_id" (EntityID Job) :> Get '[JSON] JobInfo
  :<|> "jobs" :> Capture "job_id" (EntityID Job) :> "abort"
              :> Post '[JSON] NoContent
  :<|> "jobs" :> Capture "job_id" (EntityID Job) :> Delete '[JSON] NoContent
  :<|> "jobs" :> "pending" :> Get '[JSON] [Entity Job]
  :<|> "projects" :> Capture "project_id" ProjectIdentifier :> "jobs"
                  :> Get '[JSON] [Entity Job]
  :<|> "projects" :> Capture "project_id" ProjectIdentifier :> "build"
                  :> PostCreated '[JSON] [Entity Job]

  :<|> "job_results" :> Capture "job_result_id" (EntityID JobResult)
                     :> "output"
                     :> QueryParam "offset" Integer :> Get '[PlainText] T.Text

  :<|> "projects" :> Capture "project_id" ProjectIdentifier
                  :> "artefact_token" :> Post '[PlainText] String
  :<|> "jobs" :> Capture "job_id" (EntityID Job)
              :> "artefact_token" :> Post '[PlainText] String
  :<|> "artefacts" :> Capture "token" String :> CaptureAll "path" FilePath
                   :> Get '[Artefact] BSL.ByteString

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
  :<|> abortJobAction
  :<|> deleteJobAction
  :<|> getPendingJobsAction
  :<|> getJobsOfProjectAction
  :<|> generateJobsAction

  :<|> getJobResultOutputAction

  :<|> getArtefactOfProjectAction
  :<|> getArtefactOfJobAction
  :<|> readArtefactAction

server :: ServerT CookhouseAPI SubAction
server mToken mName = enter (Nat (actionToSubAction mToken mName)) subserver

app :: Environment -> Application
app env = corsMiddleware (envConfig env) $
  serve (Proxy :: Proxy CookhouseAPI)
        (enter (Nat (subActionToHandler env)) server)
