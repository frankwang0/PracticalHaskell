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
getSpecialPairs = select . from $ \(InnerJoin user article) -> do
    on (user ^. UserId ==. article ^. ArticleAuthorId)
    where_ (((user ^. UserName `like` e) &&. (article ^. ArticleTitle `like` e))
        ||. ((user ^. UserName `like` t) &&. (article ^. ArticleTitle `like` t)))
    return (user, article)
    where
        e = val "E" ++. (%)
        t = val "T" ++. (%)

getCommentsFromUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsFromUser userId = select . from $ \comment -> do
    where_ (comment ^. CommentUserId ==. val userId)
    orderBy [asc(comment ^. CommentArticleId), desc(comment ^. CommentSubmittedAt)]
    return comment

getCommentsOnUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsOnUser userId = undefined
