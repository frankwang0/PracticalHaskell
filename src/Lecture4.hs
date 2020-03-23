{-# LANGUAGE OverloadedStrings #-}

module Lecture4 where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist (Entity(..), selectList)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import System.IO (Handle, hPrint)
import Data.Time(UTCTime)
import Data.Int

import Database (runAction, localConnString)
import Schema

myUser :: User
myUser = User { userName = "Chris"
                , userEmail = "chris@test.com"
                , userAge = 40 }

myArticle :: Article
myArticle = Article { articleTitle = "Intro to Haskell"
                      , articleBody = "The body"
                      , articlePublishedAt = read "2019-03-12 12:40:00 UTC"::UTCTime}

myUserEntity :: Entity User
myUserEntity = Entity (toSqlKey 1) myUser

myArticleEntity :: Entity Article
myArticleEntity = Entity (toSqlKey 1) myArticle

fetchAllUsers :: IO [Entity User]
fetchAllUsers = runAction localConnString $ selectList [] []

printAllKeys :: Handle -> IO ()
printAllKeys handle = do
    users <- fetchAllUsers
    printKeys handle users

printKeys :: Handle -> [Entity User] -> IO ()
printKeys handle [] = return ()
printKeys handle (x:xs) = do
    hPrint handle $ getUserId x
    printKeys handle xs

getUserId :: Entity User -> Int64
getUserId (Entity key user) = fromSqlKey key
