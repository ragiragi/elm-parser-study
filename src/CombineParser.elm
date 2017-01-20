module CombineParser exposing (..)

{-| functional parser study with elm-combine library
-}

import Combine exposing (..)
import Combine.Num exposing (int)


eval : String -> String
eval inp =
    case (parse expr inp) of
        Err _ ->
            "err"

        Ok ( _, inputStream, v ) ->
            inputStream.data ++ " = " ++ (toString v)


token : Parser s a -> Parser s a
token p =
    whitespace *> p <* whitespace


natural : Parser s Int
natural =
    token int


symbol : String -> Parser s String
symbol str =
    token (string str)


factor : Parser s Int
factor =
    (symbol "(" *> expr <* symbol ")") <|> natural


term : Parser s Int
term =
    lazy <| \() -> chainl (symbol "*" $> (*)) factor


expr : Parser s Int
expr =
    lazy <| \() -> chainl (symbol "+" $> (+)) term
