module Okapi.QuasiQuotes where

{-
newtype URL = URL Text

data URLParam = forall a. ToHttpApiData a => URLParam a

-- Maybe should just generate parser? But I thought the point was to get info for URL?
-- Maybe instead of generating URL have URL builder helpers?
data Route input = Route
  { parser :: Okapi Response
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

TODO: What TO DO with partial URLs???????????

cons :: URL -> URL -> URL
cons Empty Empty            = Empty
cons Empty pp@(PathPiece _) = Path Empty pp
cons Empty p@(Path _ _)     = p
cons pp1@(Path _ _) 

-- Expand on type safe URL building. Will need it either way

data Genre = Fantasy | SciFi | NonFiction | Romance

-- booksRoute :: ? -- Type isn't known until code is generated at compile time
booksRoute = [Okapi.route|GET /books /{Maybe BookID} ?author{Maybe Author} ?genre{Maybe Genre}|]

booksURL :: URL
booksURL = booksRoute.url (Nothing, Just "Mark Twain", Nothing)

booksRouteNamed = [Okapi.genRoute|GET /books /:bookID ?author:authorName ?genre:|]

newProduct :: Okapi Response
newProduct = do
  validProductID <- [Okapi.genParser|POST /products/{ProductID|isValid}|]
  ...

books :: Okapi Response
books = booksRoute.parser >>= booksHandler
  where
    booksHandler :: (Maybe BookID, Maybe Author, Maybe Genre) -> Okapi Response
    booksHandler (maybeBookID, maybeAuthor, maybeGenre) = do
      ...

isModern :: Date -> Bool

childActors :: [Actor] -> [Actor]

bornInIndiana :: [Actor] -> [Actor]

-- Could be gen parser
-- parser generator for headers?
moviesRoute = [Okapi.route|
  GET HEAD
  /movies
  /{Date|isModern}
  ?director{Director}
  ?actors{[Actor]->childActors->bornInIndiana|notEmpty}
  ?female
|]

THE ABOVE MAY BE PARTIAL ROUTES WITHOUT EVERY PIECE OF A URL

---------------------
-- Multiple Routes --
---------------------

[Okapi.genApp|
  [megaApp]
    [app]
      homeRoute     = GET /                                        >> getHome
      moviesRoute   = GET /movies             >> authenticate      >> getMoviesHandler
      putMovieRoute = PUT /movies /{MovieID} >>= authenticateData >>= putMovieHandler
      getBooksRoute = GET /books              >> authenticate      >> getBooksHandler
      putBookRoute  = PUT /books /{BookID}   >>= authenticateData >>= putBookHandler

    [anotherApp]
      anotherRoute = GET /anotherPath >> anotherHandler

    someOtherRoute = ...
|]

authenticate :: Okapi ()

authenticateData :: forall a. a -> Okapi a

getMoviesHandler :: Okapi Response
getBooksHandler  :: Okapi Response
putMovieHandler  :: MovieID -> Okapi Response
putBookHandler   :: BookID  -> Okapi Response
anotherHandler   :: Okapi Response

-- Generates:
megaApp :: Okapi Response
megaApp = app <|> anotherApp

app :: Okapi Response -- top level parser
app = getMovieRoute.parser <|> putMovieRoute.parser <|> ...

anotherApp :: Okapi Response
anotherApp = anotherRoute.parser

getMoviesRoute
putMovieRoute
getBooksRoute
putBookRoute
anotherRoute

TODO: generate Haddock documentation too!
https://gitlab.haskell.org/ghc/ghc/-/commit/8a59f49ae2204dbf58ef50ea8c0a50ee2c7aa64a
-}

-- genRoute creates a full route. Must have at least a method and return a response handler.
genRoute :: Q Exp
genRoute = undefined

-- Can use any part of the routing syntax to create a parser, but no URL is created just a parser.
genParser :: Q Exp
genParser = undefined

-- Generate entire top level parser. Basically multiple routes
genApp :: Q [Dec]
genApp = undefined

genOptimalApp :: Q [Dec]
genOptimalApp = undefined
