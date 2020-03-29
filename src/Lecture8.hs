{-# LANGUAGE OverloadedStrings #-}

module Lecture8 where

import Control.Monad.Logger (LoggingT)
import Data.Time (UTCTime(..), Day(..))
import Database.Persist (Entity(..), Key)
import Database.Persist.Postgresql (SqlPersistT)
import Database.Esqueleto
import Data.Time

import Database
import Schema

year :: Integer -> UTCTime
year n = UTCTime { utctDay = fromGregorian n 1 1, utctDayTime = 0 } 

lastYearsArticles :: SqlPersistT (LoggingT IO) [Entity Article]
lastYearsArticles = select . from $ \article -> do
    where_ ((article ^. ArticlePublishedAt >=. val (year 2018))
        &&. (article ^. ArticlePublishedAt <. val (year 2019)))
    orderBy [asc(article ^. ArticleTitle)]
    return article

getYoungUsers :: SqlPersistT (LoggingT IO) [Entity User]
getYoungUsers = select . from $ \user -> do
    where_ (user ^. UserAge <. val 23)
    orderBy [desc(user ^. UserName)]
    limit 10
    return user

getSpecialPairs :: SqlPersistT (LoggingT IO) [(Entity User, Entity Article)]
getSpecialPairs = undefined

getCommentsFromUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsFromUser userId = undefined

getCommentsOnUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsOnUser userId = undefined
