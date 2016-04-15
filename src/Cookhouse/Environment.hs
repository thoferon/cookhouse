module Cookhouse.Environment
  ( Environment(..)
  , defaultEnvironment
  ) where

import Cookhouse.Config
import Cookhouse.Plugins.Types

data Environment = Environment
  { envAuthenticationPlugins :: [AuthenticationPlugin]
  , envConfig                :: Config
  }

defaultEnvironment :: Config -> Environment
defaultEnvironment config = Environment
  { envAuthenticationPlugins = []
  , envConfig                = config
  }
