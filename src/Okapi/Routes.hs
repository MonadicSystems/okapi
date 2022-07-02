module Okapi.Routes where

import Data.Text

{-
Use quasiquotation to automatically generate routes:

[routes|GET /films -> getFilms|]

getFilmsHandler :: Okapi Result
getFilmsHandler = ...

Creates a record containing two functions:

`getFilms.parser` and `getFilms.url`

To insert path and query params:

[route|getFilmsByID: GET /films/<Int> -> getFilmsByIDHandler|]

getFilmsByIDHandler :: Int -> Okapi Result
getFilmsByIDHandler = ...

Creates two functions: getFilmsByID.parser and getFilmsByID.url

## Multiple Routes

```haskell
[routes|
films:
    GET   /films       -> getFilmsHandler
    POST  /films       -> createFilmHandler
    GET   /films/<Int> -> getFilmsByIDHandler
    PATCH /films/<Int> -> updateFilmHandler
|]

getFilmsHandler :: Okapi Result
getFilmsHandler = ...

createFilmHandler :: Okapi Result
createFilmHandler = ...

getFilmsByIDHandler :: Int -> Okapi Result
getfilmsByIDHandler = ...

updateFilmHandler :: Int -> Okapi Result
updateFilmHandler = ...
```

Generates a record `films` with the following fields:

```haskell
films
```

```haskell
[routes|
app:
    films:
        GET   /films       -> getFilms
        POST  /films       -> createFilm
        GET   /films/<Int> -> getFilmsByID
        PATCH /films/<Int> -> updateFilm
    actors:
        GET   /actors       -> getActors
        POST  /actors       -> createActor
        GET   /actors/<Int> -> getActorsByID
        PATCH /actors/<Int> -> updateActor
|]
```
-}
