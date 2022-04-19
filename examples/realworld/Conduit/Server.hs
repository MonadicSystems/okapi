{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Server where

import Conduit.Auth
import qualified Conduit.Database as DB
import Conduit.Type
import Control.Applicative
import Control.Monad.Combinators
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Int
import Data.Text
import Data.Time
import GHC.Generics
import Hasql.Session (QueryError)
import Okapi (OkapiT (..), Result)
import qualified Okapi

type Okapi a = OkapiT Handler a

conduit :: Okapi Okapi.Result
conduit = do
  Okapi.seg "api"
  choice
    [ users,
      user,
      profiles,
      articles,
      tags
    ]

users = do
  Okapi.post
  Okapi.seg "users"
  login <|> register

login = do
  Okapi.seg "login"
  loginData <- Okapi.bodyJSON
  handleQuery $ DB.loginUser loginData

register = do
  registerData <- Okapi.bodyJSON
  handleQuery $ DB.registerUser registerData

user = do
  Okapi.seg "user"
  userID <- authorize
  currentUser userID <|> updateUser userID

currentUser userID = do
  Okapi.get
  handleQuery $ DB.getCurrentUser userID

updateUser userID = do
  Okapi.put
  updateUserData <- Okapi.bodyJSON @UpdateUser
  handleQuery $ DB.updateUser userID updateUserData

profiles = do
  Okapi.seg "profiles"
  username <- Okapi.segParam
  profile username <|> (Okapi.seg "follow" >> follow username <|> unfollow username)

profile username = do
  Okapi.get
  mbUserID <- optional authorize
  handleQuery $ DB.getProfile mbUserID username

follow username = do
  Okapi.post
  userID <- authorize
  handleQuery $ DB.followUser userID username

unfollow username = do
  Okapi.delete
  userID <- authorize
  handleQuery $ DB.unfollowUser userID username

articles = do
  Okapi.seg "articles"
  choice
    [ Okapi.get
        >> choice
          [ feed,
            comments,
            article,
            global
          ],
      Okapi.post
        >> choice
          [ createComment,
            favoriteArticle,
            createArticle
          ],
      updateArticle,
      Okapi.delete
        >> choice
          [ deleteComment,
            unfavoriteArticle,
            deleteArticle
          ]
    ]

global = do
  mbUserID <- optional authorize
  articlesQueryTag <- optional $ Okapi.queryParam "tag"
  articlesQueryAuthor <- optional $ Okapi.queryParam "author"
  articlesQueryFavorited <- optional $ Okapi.queryParam "favorited"
  articlesQueryLimit <- option 20 $ Okapi.queryParam "limit"
  articlesQueryOffset <- option 0 $ Okapi.queryParam "offset"
  handleQuery $ DB.getArticles mbUserID ArticlesQuery {..}

feed = do
  Okapi.seg "feed"
  userID <- authorize
  limit <- option 20 $ Okapi.queryParam "limit"
  offset <- option 0 $ Okapi.queryParam "offset"
  handleQuery $ DB.feedArticles userID limit offset

article = do
  slug <- Okapi.segParam
  handleQuery $ DB.getArticle slug

comments = do
  slug <- Okapi.segParam
  Okapi.seg "comments"
  mbUserID <- optional authorize
  handleQuery $ DB.getComments mbUserID slug

createArticle = do
  userID <- authorize
  createArticleData <- Okapi.bodyJSON
  handleQuery $ DB.createArticle userID createArticleData

createComment = do
  slug <- Okapi.segParam
  Okapi.seg "comments"
  userID <- authorize
  createCommentData <- Okapi.bodyJSON
  handleQuery $ DB.createComment userID slug createCommentData

favoriteArticle = do
  slug <- Okapi.segParam
  Okapi.seg "favorite"
  userID <- authorize
  handleQuery $ DB.favoriteArticle userID slug

updateArticle = do
  Okapi.put
  slug <- Okapi.segParam
  userID <- authorize
  updateArticleData <- Okapi.bodyJSON
  handleQuery $ DB.updateArticle userID slug updateArticleData

deleteArticle = do
  slug <- Okapi.segParam
  userID <- authorize
  handleQuery $ DB.deleteArticle userID slug

deleteComment = do
  slug <- Okapi.segParam
  Okapi.seg "comments"
  commentID <- Okapi.segParam
  userID <- authorize
  handleQuery $ DB.deleteComment userID slug commentID

unfavoriteArticle = do
  slug <- Okapi.segParam
  Okapi.seg "favorite"
  userID <- authorize
  handleQuery $ DB.unfavoriteArticle userID slug

tags = do
  Okapi.get
  Okapi.seg "tags"
  handleQuery DB.getTags

authorize = do
  authHeaderValue <- Okapi.auth
  jwtSecret <- grab @Text
  case extractToken authHeaderValue >>= verifyToken jwtSecret of
    Nothing -> Okapi.error401 [] ""
    Just userID -> pure userID

handleQuery :: ToJSON a => Okapi (Either QueryError a) -> Okapi Okapi.Result
handleQuery query = do
  queryResult <- query
  case queryResult of
    Left _ -> Okapi.error422 [] genericError
    Right value -> Okapi.okJSON [] value
