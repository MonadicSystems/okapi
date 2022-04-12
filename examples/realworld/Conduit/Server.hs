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
import Okapi

type Okapi a = OkapiT Handler a

conduit :: Okapi Response
conduit = do
  seg "api"
  choice
    [ users,
      user,
      profiles,
      articles,
      tags
    ]

users = do
  post
  seg "users"
  login <|> register

login = do
  seg "login"
  loginData <- bodyJSON @Login
  handleQuery $ DB.loginUser loginData

register = do
  registerData <- bodyJSON @Register
  handleQuery $ DB.registerUser registerData

user = do
  seg "user"
  userID <- authorize
  currentUser userID <|> updateUser userID

currentUser userID = do
  get
  handleQuery $ DB.getCurrentUser userID

updateUser userID = do
  put
  updateUserData <- bodyJSON @UpdateUser
  handleQuery $ DB.updateUser userID updateUserData

profiles = do
  seg "profiles"
  username <- segParam
  profile username <|> (seg "follow" >> follow username <|> unfollow username)

profile username = do
  get
  mbUserID <- optional authorize
  handleQuery $ DB.getProfile mbUserID username

follow username = do
  post
  userID <- authorize
  handleQuery $ DB.followUser userID username

unfollow username = do
  delete
  userID <- authorize
  handleQuery $ DB.unfollowUser userID username

articles = do
  seg "articles"
  choice
    [ get
        >> choice
          [ feed,
            comments,
            article,
            global
          ],
      post
        >> choice
          [ createComment,
            favoriteArticle,
            createArticle
          ],
      updateArticle,
      delete
        >> choice
          [ deleteComment,
            unfavoriteArticle,
            deleteArticle
          ]
    ]

global = do
  mbUserID <- optional authorize
  articlesQueryTag <- optional $ queryParam @Text "tag"
  articlesQueryAuthor <- optional $ queryParam @Text "author"
  articlesQueryFavorited <- optional $ queryParam @Text "favorited"
  articlesQueryLimit <- option 20 $ queryParam @Int32 "limit"
  articlesQueryOffset <- option 0 $ queryParam @Int32 "offset"
  handleQuery $ DB.getArticles mbUserID ArticlesQuery {..}

feed = do
  seg "feed"
  userID <- authorize
  limit <- option 20 $ queryParam @Int32 "limit"
  offset <- option 0 $ queryParam @Int32 "offset"
  handleQuery $ DB.feedArticles userID limit offset

article = do
  slug <- segParam @Text
  handleQuery $ DB.getArticle slug

comments = do
  slug <- segParam @Text
  seg "comments"
  mbUserID <- optional authorize
  handleQuery $ DB.getComments mbUserID slug

createArticle = do
  userID <- authorize
  createArticleData <- bodyJSON @CreateArticle
  handleQuery $ DB.createArticle userID createArticleData

createComment = do
  slug <- segParam @Text
  seg "comments"
  userID <- authorize
  createCommentData <- bodyJSON @CreateComment
  handleQuery $ DB.createComment userID slug createCommentData

favoriteArticle = do
  slug <- segParam @Text
  seg "favorite"
  userID <- authorize
  handleQuery $ DB.favoriteArticle userID slug

updateArticle = do
  put
  slug <- segParam @Text
  userID <- authorize
  updateArticleData <- bodyJSON @UpdateArticle
  handleQuery $ DB.updateArticle userID slug updateArticleData

deleteArticle = do
  slug <- segParam @Text
  userID <- authorize
  handleQuery $ DB.deleteArticle userID slug

deleteComment = do
  slug <- segParam @Text
  seg "comments"
  commentID <- segParam @Int32
  userID <- authorize
  handleQuery $ DB.deleteComment userID slug commentID

unfavoriteArticle = do
  slug <- segParam @Text
  seg "favorite"
  userID <- authorize
  handleQuery $ DB.unfavoriteArticle userID slug

tags = do
  get
  seg "tags"
  handleQuery DB.getTags

authorize = do
  authHeaderValue <- auth
  jwtSecret <- grab @Text
  case extractToken authHeaderValue >>= verifyToken jwtSecret of
    Nothing -> abort401 [] ""
    Just userID -> pure userID

handleQuery :: ToJSON a => Okapi (Either QueryError a) -> Okapi Response
handleQuery query = do
  queryResult <- query
  case queryResult of
    Left _ -> abort422 [] genericError
    Right value -> okJSON [] value
