module Cookhouse.App
  ( app
  ) where

import Cookhouse.Actions.Types
import Cookhouse.Config
import Cookhouse.Middlewares.CORS
import Cookhouse.Routes

app :: Config -> AppSpockM ()
app config = do
  corsMiddleware config
  routes         config
