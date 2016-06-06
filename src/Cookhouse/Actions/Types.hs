{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Actions.Types where

import Database.PostgreSQL.Simple

import Web.Spock.Simple

import Cookhouse.Environment

type AppSpockM = SpockT (WebStateM Connection () Environment)

type AppSpockAction = SpockActionCtx () Connection () Environment

instance HasEnvironment AppSpockM where
  getEnvironment = getState

instance HasEnvironment AppSpockAction where
  getEnvironment = getState
