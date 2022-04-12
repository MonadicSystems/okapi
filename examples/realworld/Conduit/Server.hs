{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Okapi.Type
import Okapi.Grammar

-- type Okapi a = OkapiT Handler a

conduit :: MonadHandler m => Okapi m Response
conduit = do
  seg "api"
  choice
    [ users,
      user,
      profiles,
      articles,
      tags
    ]

users :: MonadHandler m => Okapi m Response
users = do
  post
  seg "users"
  login <|> register

login :: MonadHandler m => Okapi m Response
login = do
  seg "login"
  loginData <- bodyJSON @Login
  handleQuery $ DB.loginUser loginData

register :: MonadHandler m => Okapi m Response
register = do
  registerData <- bodyJSON @Register
  handleQuery $ DB.registerUser registerData

user :: MonadHandler m => Okapi m Response
user = do
  seg "user"
  userID <- authorize
  currentUser userID <|> updateUser userID

currentUser :: MonadHandler m => Int32 -> Okapi m Response
currentUser userID = do
  get
  handleQuery $ DB.getCurrentUser userID

updateUser :: MonadHandler m => Int32 -> Okapi m Response
updateUser userID = do
  put
  updateUserData <- bodyJSON @UpdateUser
  handleQuery $ DB.updateUser userID updateUserData

profiles :: MonadHandler m => Okapi m Response
profiles = do
  seg "profiles"
  username <- segParam
  profile username <|> (seg "follow" >> follow username <|> unfollow username)

profile :: MonadHandler m => Text -> Okapi m Response
profile username = do
  get
  mbUserID <- optional authorize
  handleQuery $ DB.getProfile mbUserID username

follow :: MonadHandler m => Text -> Okapi m Response
follow username = do
  post
  userID <- authorize
  handleQuery $ DB.followUser userID username

unfollow :: MonadHandler m => Text -> Okapi m Response
unfollow username = do
  delete
  userID <- authorize
  handleQuery $ DB.unfollowUser userID username

articles :: MonadHandler m => Okapi m Response
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

global :: MonadHandler m => Okapi m Response
global = do
  mbUserID <- optional authorize
  articlesQueryTag <- optional $ queryParam @Text "tag"
  articlesQueryAuthor <- optional $ queryParam @Text "author"
  articlesQueryFavorited <- optional $ queryParam @Text "favorited"
  articlesQueryLimit <- option 20 $ queryParam @Int32 "limit"
  articlesQueryOffset <- option 0 $ queryParam @Int32 "offset"
  handleQuery $ DB.getArticles mbUserID ArticlesQuery {..}

feed :: MonadHandler m => Okapi m Response
feed = do
  seg "feed"
  userID <- authorize
  limit <- option 20 $ queryParam @Int32 "limit"
  offset <- option 0 $ queryParam @Int32 "offset"
  handleQuery $ DB.feedArticles userID limit offset

article :: MonadHandler m => Okapi m Response
article = do
  slug <- segParam
  handleQuery $ DB.getArticle slug

comments :: MonadHandler m => Okapi m Response
comments = do
  slug <- segParam
  seg "comments"
  mbUserID <- optional authorize
  handleQuery $ DB.getComments mbUserID slug

createArticle :: MonadHandler m => Okapi m Response
createArticle = do
  userID <- authorize
  createArticleData <- bodyJSON @CreateArticle
  handleQuery $ DB.createArticle userID createArticleData

createComment :: MonadHandler m => Okapi m Response
createComment = do
  slug <- segParam
  seg "comments"
  userID <- authorize
  createCommentData <- bodyJSON @CreateComment
  handleQuery $ DB.createComment userID slug createCommentData

favoriteArticle :: MonadHandler m => Okapi m Response
favoriteArticle = do
  slug <- segParam
  seg "favorite"
  userID <- authorize
  handleQuery $ DB.favoriteArticle userID slug

updateArticle :: MonadHandler m => Okapi m Response
updateArticle = do
  put
  slug <- segParam
  userID <- authorize
  updateArticleData <- bodyJSON @UpdateArticle
  handleQuery $ DB.updateArticle userID slug updateArticleData

deleteArticle :: MonadHandler m => Okapi m Response
deleteArticle = do
  slug <- segParam
  userID <- authorize
  handleQuery $ DB.deleteArticle userID slug

deleteComment :: MonadHandler m => Okapi m Response
deleteComment = do
  slug <- segParam
  seg "comments"
  commentID <- segParam @Int32
  userID <- authorize
  handleQuery $ DB.deleteComment userID slug commentID

unfavoriteArticle :: MonadHandler m => Okapi m Response
unfavoriteArticle = do
  slug <- segParam
  seg "favorite"
  userID <- authorize
  handleQuery $ DB.unfavoriteArticle userID slug

tags :: MonadHandler m => Okapi m Response
tags = do
  get
  seg "tags"
  handleQuery DB.getTags

authorize :: MonadHandler m => Okapi m Int32
authorize = do
  authHeaderValue <- header "Authorization"
  jwtSecret <- action $ grab @Text
  err401 <- mayThrow 401 []
  case extractToken authHeaderValue >>= verifyToken jwtSecret of
    Nothing -> throw $ err401 "Not Authorized"
    Just userID -> pure userID

handleQuery :: forall m a. (MonadHandler m, ToJSON a) => m (Either QueryError a) -> Okapi m Response
handleQuery query = do
  queryResult <- action query
  err422 <- mayThrow 422 []
  ok <- mayRespondJSON @a 200 []
  case queryResult of
    Left _ -> throw $ err422 "Database error occured"
    Right value -> respond $ ok value
