{-# LANGUAGE OverloadedStrings #-}

module Lecture6 where

import Control.Monad (forM_)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), filterLogger, LogSource)
import Control.Monad.Reader (runReaderT)
import Data.List (sortBy)
import Database.Persist (Entity(..), (<.), (>.), (==.), selectList, Filter(..), SelectOpt(..))
import Database.Persist.Postgresql (withPostgresqlConn, ConnectionString, SqlPersistT, fromSqlKey)
import qualified Data.Text as T

import Database (localConnString)
import Schema

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction conn action = 
  runStdoutLoggingT $ filterLogger logFilter $
  withPostgresqlConn conn (runReaderT action)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug  = False 
logFilter _ (LevelOther _) = False

fetchSpecialUsers :: IO ()
fetchSpecialUsers = do
  users <- runAction localConnString query
  forM_ users print
  where
    query :: SqlPersistT (LoggingT IO) [Entity User]
    query = selectList
      [UserName <. "N", UserName >. "C", UserAge <. 25, UserAge >. 20]
      [Asc UserAge]

fetchSpecialPairs :: IO [(Entity User, Entity Article)]
fetchSpecialPairs = do
  -- users <- runAction localConnString userQuery
  -- where
  --   userQuery :: SqlPersistT (LoggingT IO) [Entity User]
  --   userQuery = selectList [] []
    
  -- articles <- runAction localConnString articleQuery
  -- where
  --   articleQuery :: SqlPersistT (LoggingT IO) [Entity Article]
  --   articleQuery = selectList [] []

  return []