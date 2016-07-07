{-# LANGUAGE DeriveGeneric #-}

module Cookhouse.Errors where

import GHC.Generics

import Data.Binary

import Network.HTTP.Types.Status

import Cookhouse.Capabilities

data CookhouseError
  = SQLError String
  | SQLRecordNotFoundError
  | SQLInvalidFieldError String
  | ParamError String
  | ValidationError String String
  | PermissionError CookhouseAccess
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

instance Binary CookhouseError

class Show e => HTTPError e where
  errStatusAndMsg :: e -> (Status, String)
  errStatusAndMsg _ = (internalServerError500, "Unknown error.")

instance HTTPError CookhouseError where
  errStatusAndMsg err = case err of
    SQLError _             -> (internalServerError500, "SQL Error.")
    SQLRecordNotFoundError -> (notFound404, "Record not found.")
    SQLInvalidFieldError _ -> (internalServerError500, "SQL Error.")
    ParamError msg         -> (badRequest400, msg)
    ValidationError f msg  ->
      (badRequest400, "Error on field \"" ++ f ++ "\": " ++ msg ++ ".")
    PermissionError _      -> (unauthorized401, "Permission denied.")
    AuthenticationPluginError name msg ->
      ( unauthorized401
      , "Authentication plugin \"" ++ name ++ "\" failed: " ++ msg )
    TriggerPluginError name msg ->
      ( internalServerError500
      , "Trigger plugin \"" ++ name ++ "\" failed:" ++ msg )
    SourcePluginError name msg ->
      ( internalServerError500
      , "Source plugin \"" ++ name ++ "\" failed:" ++ msg )
    StepPluginError name msg ->
      ( internalServerError500
      , "Step plugin \"" ++ name ++ "\" failed:" ++ msg )
    MissingPluginError name ->
      (badRequest400 , "Incorrect plugin name: " ++ name)
    IncorrectProjectIdentifierError identifier ->
      ( badRequest400
      , "Incorrect project identifier: " ++ identifier )
    CircularDependencyError identifier ->
      ( internalServerError500
      , "Circular dependency around " ++ identifier ++ "." )
    _ -> (internalServerError500, "Something went wrong.")

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
