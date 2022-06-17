module Okapi.Routes where

{-
Use quasiquotation to automatically generate routes:

[route|GET /films -> getFilms|]

getFilms :: Okapi Result
getFilms = ...

Creates two functions: getFilmsRoute and getFilmsURL

To insert path and query params:

[route|GET /films/<Int> -> getFilmsByID|]

getFilmsByID :: Int -> Okapi Result
getFilmsByID = ...

Creates two functions: getFilmsByIDRoute and getFilmsByIDURL
-}
