{-
   Elm Functional Parser Study
-}


module Parser exposing (..)

-- TYPES


type alias Parser a =
    String -> List ( a, String )


type alias ParserResult a =
    List ( a, String )



-- PARSERS


{-| The parser item fails if the input is empty,
    and consumes the first character otherwise:
-}
item : Parser Char
item inp =
    case (String.uncons inp) of
        Nothing ->
            []

        Just ( head, tail ) ->
            [ ( head, tail ) ]


{-| The parser failure always fails:
-}
failure : Parser a
failure inp =
    []


{-| The parser return v always succeeds,
    returning the value v without consuming any input:
-}
return : a -> Parser a
return v inp =
    [ ( v, inp ) ]


{-| The parser p +++ q behaves as the parser p if it succeeds,
    and as the parser q otherwise:
-}
(+++) : Parser a -> Parser a -> Parser a
(+++) p q =
    \inp ->
        let
            resP =
                p inp
        in
            case resP of
                [] ->
                    parse q inp

                _ ->
                    resP


{-| The function parse applies a parser to a string:
-}
parse : Parser a -> String -> List ( a, String )
parse p inp =
    p inp
