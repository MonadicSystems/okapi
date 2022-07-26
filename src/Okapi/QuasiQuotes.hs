module Okapi.QuasiQuotes where

{-
newtype URL = URL Text

-- Maybe should just generate parser? But I thought the point was to get info for URL?
-- Maybe instead of generating URL have URL builder helpers?
data Route input output = Route
  { parser :: Okapi output
  , url    :: input -> URL
  }

sequencing routes
route1 <> route2 = Route
  { parser = do
      route1
      route2
  , url = 
  }

data RouteParams = forall a. RouteParams (Map Text a)

-- (%%) :: (a -> URL) -> (b -> URL) -> (a -> b -> URL)
-- (%%) urlProducer urlProducer' = \a -> \b -> urlProducer a <> urlProducer' b

-- Example Usage

newtype BookID = BookID Int deriving (FromHttpApiData)

newtype Author = Author Text deriving (FromHttpApiData)

data URL = Empty | PathPiece Text | QueryPiece Text Text | Path URL URL

cons :: URL -> URL -> URL
cons Empty Empty            = Empty
cons Empty pp@(PathPiece _) = Path Empty pp
cons Empty p@(Path _ _)     = p
cons pp1@(Path _ _) 

-- Expand on type safe URL building. Will need it either way

data Genre = Fantasy | SciFi | NonFiction | Romance

-- booksRoute :: ? -- Type isn't known until code is generated at compile time
booksRoute = [Okapi.route|GET /books/:Maybe BookID?author=:Maybe Author&genre=:Maybe Genre|]

booksURL :: URL
booksURL = booksRoute.url (Nothing, Just "Mark Twain", Nothing)

authenticate :: a -> Okapi a
authenticate = undefined

books :: Okapi Response
books = booksRoute.parser >>= booksHandler
  where
    booksHandler :: (Maybe BookID, Maybe Author, Maybe Genre) -> Okapi Response
    booksHandler (maybeBookID, maybeAuthor, maybeGenre) = do
      ...

isModern :: Date -> Bool

childActorsFilter :: [Actor] -> [Actor]

-- Could be gen parser
-- parser generator for headers?
moviesRoute = [Okapi.route|
  GET,HEAD
  /movies/{Date|isModern}
  ?director={Director}&actors={[Actor]->childActorsFilter}
|]

---------------------
-- Multiple Routes --
---------------------

getMovies :: Okapi Response
getBooks  :: Okapi Response
putMovie  :: MovieID -> Okapi Response
putBook   :: BookID  -> Okapi Response

[Okapi.module|
  moviesRoute = do
    GET /movies
    returnMovies
  putMovieRoute = do
    PUT /movies/:MovieID >>= putMovie >> succeed
  getBooksRoute = do
    GET /books

  putBookRoute = do
    PUT /books/:BookID
|]

-- Generates:
app -- top level parser
getMoviesRoute
putMovieRoute
getBooksRoute
putBookRoute
-}
