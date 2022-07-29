{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, toJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic, Par1)
import Okapi
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (ToHttpApiData)
import Web.Internal.HttpApiData

{-
We're going to build a Todo API with the following endpoints:

GET /
Health check. Returns 200 response with "OK" as the body.

GET /todos
Returns all todos as JSON. Has optional query param "status" to filter todos with that status.

GET /todos/:uid
Returns the todo with the matching :uid as JSON.

POST /todos
Accepts a todo/todos encoded as JSON to store on the server.

PATCH /todos/:uid
Accepts todo information as JSON. For updating a todo with the :uid.

DELETE /todos/:uid
Deletes the todo with the matching :uid from the server.
-}

-- TYPES --

data Todo = Todo
  { todoID :: Int,
    todoName :: Text,
    todoStatus :: Status
  }
  deriving (Eq, Ord, Generic, ToJSON, Show)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

data TodoForm = TodoForm
  { todoFormName :: Text,
    todoFormStatus :: Status
  }
  deriving (Eq, Ord, Generic, FromForm, Show)

instance ToRow TodoForm where
  toRow (TodoForm name status) = toRow (name, status)

data Status
  = Incomplete
  | Archived
  | Complete
  deriving (Eq, Ord, Show)

instance ToJSON Status where
  toJSON Incomplete = "incomplete"
  toJSON Archived = "archived"
  toJSON Complete = "complete"

instance FromHttpApiData Status where
  parseQueryParam "incomplete" = Right Incomplete
  parseQueryParam "archived" = Right Archived
  parseQueryParam "complete" = Right Complete
  parseQueryParam _ = Left "Incorrect format for Status value"

instance ToField Status where
  toField status =
    case status of
      Incomplete -> SQLText "incomplete"
      Archived -> SQLText "archived"
      Complete -> SQLText "complete"

instance FromField Status where
  fromField field = do
    case fieldData field of
      SQLText "incomplete" -> pure Incomplete
      SQLText "archived" -> pure Archived
      SQLText "complete" -> pure Complete
      _ -> returnError ConversionFailed field "Couldn't get Status value from field"

type Okapi = OkapiT IO

-- MAIN --

main :: IO ()
main = do
  conn <- open "todo.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY, name TEXT, status TEXT)"
  runOkapi id notFound 3000 (todoAPI conn)
  close conn

-- SERVER FUNCTIONS

todoAPI :: Connection -> Okapi Response
todoAPI conn =
  healthCheck
    <|> getTodo conn
    <|> getAllTodos conn
    <|> createTodo conn
    <|> editTodo conn
    <|> forgetTodo conn

healthCheck :: Okapi Response
healthCheck = do
  get
  optional $ pathSeg ""
  respond ok

getTodo :: Connection -> Okapi Response
getTodo conn = do
  get
  pathSeg "todos"
  todoID <- pathParam @Int
  maybeTodo <- liftIO $ selectTodo conn todoID
  case maybeTodo of
    Nothing -> throw internalServerError
    Just todo -> ok & aeson todo & respond

getAllTodos :: Connection -> Okapi Response
getAllTodos conn = do
  get
  pathSeg "todos"
  status <- optional $ queryParam @Status "status"
  todos <- liftIO $ selectAllTodos conn status
  ok & aeson todos & respond

createTodo :: Connection -> Okapi Response
createTodo conn = do
  post
  pathSeg "todos"
  todoForm <- bodyForm
  liftIO $ insertTodoForm conn todoForm
  respond ok

editTodo :: Connection -> Okapi Response
editTodo conn = do
  put
  pathSeg "todos"
  todoID <- pathParam @Int
  todoForm <- bodyForm @TodoForm
  liftIO $ updateTodo conn todoID todoForm
  respond ok

forgetTodo :: Connection -> Okapi Response
forgetTodo conn = do
  delete
  pathSeg "todos"
  todoID <- pathParam @Int
  liftIO $ deleteTodo conn todoID
  respond ok

-- DATABASE FUNCTIONS

insertTodoForm :: Connection -> TodoForm -> IO ()
insertTodoForm conn = execute conn "INSERT INTO todos (name, status) VALUES (?, ?)"

selectTodo :: Connection -> Int -> IO (Maybe Todo)
selectTodo conn todoID = listToMaybe <$> query conn "SELECT * FROM todos WHERE id = ?" (Only todoID)

selectAllTodos :: Connection -> Maybe Status -> IO [Todo]
selectAllTodos conn maybeStatus = case maybeStatus of
  Nothing -> query_ conn "SELECT * FROM todos"
  Just status -> query conn "SELECT * FROM todos WHERE status = ?" (Only status)

updateTodo :: Connection -> Int -> TodoForm -> IO ()
updateTodo conn todoID TodoForm {..} =
  executeNamed
    conn
    "UPDATE todos SET name = :name, status = :status WHERE id = :id"
    [":id" := todoID, ":name" := todoFormName, ":status" := todoFormStatus]

deleteTodo :: Connection -> Int -> IO ()
deleteTodo conn todoID = execute conn "DELETE FROM todos WHERE id = ?" (Only todoID)
