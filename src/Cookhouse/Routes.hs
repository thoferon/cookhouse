module Cookhouse.Routes where

import qualified Web.Spock.Simple as S

import           Cookhouse.Actions.Types
import           Cookhouse.Config
import           Cookhouse.Middlewares.CORS

import           Cookhouse.Actions.ProjectActions
import           Cookhouse.Actions.SessionActions

routes :: Config -> AppSpockM ()
routes config = do
  let get    route action = S.get    route $ setCorsHeaders config >> action
      post   route action = S.post   route $ setCorsHeaders config >> action
      put    route action = S.put    route $ setCorsHeaders config >> action
      delete route action = S.delete route $ setCorsHeaders config >> action

  post "/signin"  signinAction
  post "/signout" signoutAction

  get "/projects" getProjectsAction
