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

-- type Okapi a = OkapiT Handler a

conduit :: (MonadOkapi m, MonadHandler m) => m Response
conduit = do
  seg "api"
  choice
    [ users,
      user,
      profiles,
      articles,
      tags
    ]

users :: (MonadOkapi m, MonadHandler m) => m Response
users = do
  post
  seg "users"
  login <|> register

login :: (MonadOkapi m, MonadHandler m) => m Response
login = do
  seg "login"
  loginData <- bodyJSON @Login
  handleQuery $ DB.loginUser loginData

register :: (MonadOkapi m, MonadHandler m) => m Response
register = do
  registerData <- bodyJSON @Register
  handleQuery $ DB.registerUser registerData

user :: (MonadOkapi m, MonadHandler m) => m Response
user = do
  seg "user"
  userID <- authorize
  currentUser userID <|> updateUser userID

currentUser :: (MonadOkapi m, MonadHandler m) => Int32 -> m Response
currentUser userID = do
  get
  handleQuery $ DB.getCurrentUser userID

updateUser :: (MonadOkapi m, MonadHandler m) => Int32 -> m Response
updateUser userID = do
  put
  updateUserData <- bodyJSON @UpdateUser
  handleQuery $ DB.updateUser userID updateUserData

profiles :: (MonadOkapi m, MonadHandler m) => m Response
profiles = do
  seg "profiles"
  username <- segParam
  profile username <|> (seg "follow" >> follow username <|> unfollow username)

profile :: (MonadOkapi m, MonadHandler m) => Text -> m Response
profile username = do
  get
  mbUserID <- optional authorize
  handleQuery $ DB.getProfile mbUserID username

follow :: (MonadOkapi m, MonadHandler m) => Text -> m Response
follow username = do
  post
  userID <- authorize
  handleQuery $ DB.followUser userID username

unfollow :: (MonadOkapi m, MonadHandler m) => Text -> m Response
unfollow username = do
  delete
  userID <- authorize
  handleQuery $ DB.unfollowUser userID username

articles :: (MonadOkapi m, MonadHandler m) => m Response
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

global :: (MonadOkapi m, MonadHandler m) => m Response
global = do
  mbUserID <- optional authorize
  articlesQueryTag <- optional $ queryParam "tag"
  articlesQueryAuthor <- optional $ queryParam "author"
  articlesQueryFavorited <- optional $ queryParam "favorited"
  articlesQueryLimit <- option 20 $ queryParamAs @Int32 "limit"
  articlesQueryOffset <- option 0 $ queryParamAs @Int32 "offset"
  handleQuery $ DB.getArticles mbUserID ArticlesQuery {..}

feed :: (MonadOkapi m, MonadHandler m) => m Response
feed = do
  seg "feed"
  userID <- authorize
  limit <- option 20 $ queryParamAs @Int32 "limit"
  offset <- option 0 $ queryParamAs @Int32 "offset"
  handleQuery $ DB.feedArticles userID limit offset

article :: (MonadOkapi m, MonadHandler m) => m Response
article = do
  slug <- segParam
  handleQuery $ DB.getArticle slug

comments :: (MonadOkapi m, MonadHandler m) => m Response
comments = do
  slug <- segParam
  seg "comments"
  mbUserID <- optional authorize
  handleQuery $ DB.getComments mbUserID slug

createArticle :: (MonadOkapi m, MonadHandler m) => m Response
createArticle = do
  userID <- authorize
  createArticleData <- bodyJSON @CreateArticle
  handleQuery $ DB.createArticle userID createArticleData

createComment :: (MonadOkapi m, MonadHandler m) => m Response
createComment = do
  slug <- segParam
  seg "comments"
  userID <- authorize
  createCommentData <- bodyJSON @CreateComment
  handleQuery $ DB.createComment userID slug createCommentData

favoriteArticle :: (MonadOkapi m, MonadHandler m) => m Response
favoriteArticle = do
  slug <- segParam
  seg "favorite"
  userID <- authorize
  handleQuery $ DB.favoriteArticle userID slug

updateArticle :: (MonadOkapi m, MonadHandler m) => m Response
updateArticle = do
  put
  slug <- segParam
  userID <- authorize
  updateArticleData <- bodyJSON @UpdateArticle
  handleQuery $ DB.updateArticle userID slug updateArticleData

deleteArticle :: (MonadOkapi m, MonadHandler m) => m Response
deleteArticle = do
  slug <- segParam
  userID <- authorize
  handleQuery $ DB.deleteArticle userID slug

deleteComment :: (MonadOkapi m, MonadHandler m) => m Response
deleteComment = do
  slug <- segParam
  seg "comments"
  commentID <- segParamAs @Int32
  userID <- authorize
  handleQuery $ DB.deleteComment userID slug commentID

unfavoriteArticle :: (MonadOkapi m, MonadHandler m) => m Response
unfavoriteArticle = do
  slug <- segParam
  seg "favorite"
  userID <- authorize
  handleQuery $ DB.unfavoriteArticle userID slug

tags :: (MonadOkapi m, MonadHandler m) => m Response
tags = do
  get
  seg "tags"
  handleQuery DB.getTags

authorize :: (MonadOkapi m, MonadHandler m) => m Int32
authorize = do
  authHeaderValue <- auth
  jwtSecret <- grab @Text
  case extractToken authHeaderValue >>= verifyToken jwtSecret of
    Nothing -> abort401 [] ""
    Just userID -> pure userID

handleQuery :: (MonadOkapi m, MonadHandler m, ToJSON a) => m (Either QueryError a) -> m Response
handleQuery query = do
  queryResult <- query
  case queryResult of
    Left _ -> abort422 [] genericError
    Right value -> respondJSON [] value
