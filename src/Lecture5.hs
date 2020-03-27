{-# LANGUAGE OverloadedStrings #-}

module Lecture5 where

import Control.Monad.Logger (LoggingT)
import           Control.Monad
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime(..), Day(..), fromGregorian)
import Database.Persist (insert, delete, get, selectList, Key, (<.), SelectOpt(..))
import Database.Persist.Postgresql (SqlPersistT, Entity(..), toSqlKey, (>.), (<.), (==.), (>=.))

import Database (runAction, localConnString)
import Schema

myUser :: User
myUser = User "Lecture 5 User" "lec5@user.com" 23

insertAndPrintKey :: IO ()
insertAndPrintKey = void $ runAction localConnString myQuery
  where
    myQuery :: SqlPersistT (LoggingT IO) (Key User)
    myQuery = insert myUser

deleteNewUser :: IO ()
deleteNewUser = runAction localConnString myQuery
  where 
    myQuery = delete (toSqlKey 103 :: Key User)

fetch100 :: IO (Text, UTCTime)
fetch100 = do
  article <- runAction localConnString fetchQuery
  return (articleTitle $ fromJust article, articlePublishedAt $ fromJust article)
  where
    fetchQuery :: SqlPersistT (LoggingT IO) (Maybe Article)
    fetchQuery = get (toSqlKey 100)

lastYearsArticles :: IO [Entity Article]
lastYearsArticles = runAction localConnString query
  where
    query = selectList 
                      [ ArticlePublishedAt >=. year 2018
                      , ArticlePublishedAt <. year 2019] 
                      [Asc ArticleTitle]
    year n = UTCTime 
                    { utctDay = fromGregorian n 1 1
                    , utctDayTime = 0 } 

getYoungUsers :: IO [Entity User]
getYoungUsers = runAction localConnString query
  where
    query = selectList  [UserAge <. 23] 
                        [LimitTo 10, Desc UserName]
