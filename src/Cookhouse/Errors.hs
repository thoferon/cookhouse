module Cookhouse.Errors where

import GHC.Generics -- FIXME: huh ?!

import Data.Aeson

import Database.Seakale.Types

import Servant

import Cookhouse.Capabilities

data CookhouseError
  = SQLError SeakaleError
  | ParamError String
  | ValidationError String String
  | PermissionError CookhouseAccess
  | InvalidCredentials
  | AuthenticationPluginError String String
  | TriggerPluginError String String
  | SourcePluginError String String
  | StepPluginError String String
  | StepFailed String
  | MissingPluginError String
  | IncorrectProjectIdentifierError String
  | CircularDependencyError String
  | IOError String
  deriving (Eq, Show, Generic)

toServantError :: CookhouseError -> ServantErr
toServantError err =
  let (serr, msg) = errStatusAndMsg err
  in serr { errBody = encode $ object [ "error" .= msg ] }

-- FIXME: not needed anymore
class Show e => HTTPError e where
  errStatusAndMsg :: e -> (ServantErr, String)
  errStatusAndMsg _ = (err500, "Unknown error.")

instance HTTPError CookhouseError where
  errStatusAndMsg = \case
    SQLError EntityNotFoundError -> (err404, "Record not found.")
    SQLError _ -> (err500, "SQL Error.")
    ParamError msg -> (err400, msg)
    ValidationError f msg  ->
      (err400, "Error on field \"" ++ f ++ "\": " ++ msg ++ ".")
    PermissionError _ -> (err401, "Permission denied.")
    InvalidCredentials -> (err401, "Invalid credentials.")
    AuthenticationPluginError name msg ->
      (err401 , "Authentication plugin \"" ++ name ++ "\" failed: " ++ msg)
    TriggerPluginError name msg ->
      (err500 , "Trigger plugin \"" ++ name ++ "\" failed:" ++ msg)
    SourcePluginError name msg ->
      (err500 , "Source plugin \"" ++ name ++ "\" failed:" ++ msg)
    StepPluginError name msg ->
      (err500 , "Step plugin \"" ++ name ++ "\" failed:" ++ msg)
    MissingPluginError name -> (err400 , "Incorrect plugin name: " ++ name)
    IncorrectProjectIdentifierError identifier ->
      (err400, "Incorrect project identifier: " ++ identifier)
    CircularDependencyError identifier ->
      (err500, "Circular dependency around " ++ identifier ++ ".")
    _ -> (err500, "Something went wrong.")

cantBeBlankError :: String -> CookhouseError
cantBeBlankError = flip ValidationError "Can't be blank"

cantBeEmptyError :: String -> CookhouseError
cantBeEmptyError = flip ValidationError "Can't be empty"

alreadyTakenError :: String -> CookhouseError
alreadyTakenError = flip ValidationError "Already taken"

notFoundError :: String -> CookhouseError
notFoundError = flip ValidationError "Not found"

isInvalidError :: String -> CookhouseError
isInvalidError = flip ValidationError "Is invalid"
