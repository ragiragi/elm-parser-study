module Tests exposing (..)

import Test exposing (..)
import Parser exposing (..)
import Char
import Expect


--import Fuzz exposing (list, int, tuple, string)


all : Test
all =
    describe "Elm Functional Parser Test Suite"
        [ describe "Basic Parsers"
            [ describe "item"
                [ test "extracts first character" <|
                    \() ->
                        Expect.equal (parse item "abc") [ ( 'a', "bc" ) ]
                , test "return empty if string is empty" <|
                    \() ->
                        Expect.equal (parse item "") []
                ]
            , describe "failure"
                [ test "always returns empty" <|
                    \() ->
                        Expect.equal (parse failure "abc") []
                ]
            , describe "return"
                [ test "returns given value" <|
                    \() ->
                        let
                            p1 =
                                return True
                        in
                            Expect.equal (parse p1 "abc") [ ( True, "abc" ) ]
                ]
            ]
        , describe "(+++)"
            [ test "returns first result if it succeeded" <|
                \() ->
                    let
                        p1 =
                            item +++ (return 'd')
                    in
                        Expect.equal (p1 "abc") [ ( 'a', "bc" ) ]
            , test "return second result if it failed" <|
                \() ->
                    let
                        p1 =
                            failure +++ (return 'd')
                    in
                        Expect.equal (parse p1 "abc") [ ( 'd', "abc" ) ]
            ]
        , describe "(>>=)"
            [ test "sequence two parsers" <|
                \() ->
                    let
                        p1 =
                            item >>= (\a -> return (Char.toUpper a))
                    in
                        Expect.equal (parse p1 "abc") [ ( 'A', "bc" ) ]
            ]
        , describe "andThen"
            [ test "sequence two parsers" <|
                \() ->
                    let
                        p1 =
                            andThen (\a -> return (Char.toUpper a)) item
                    in
                        Expect.equal (parse p1 "abc") [ ( 'A', "bc" ) ]
            ]
        , describe "sequence"
            [ test "sequence three parsers" <|
                \() ->
                    let
                        p1 =
                            do [ item, item, item ] >>= takeHeadAndLast
                    in
                        Expect.equal
                            (parse p1 "abcdef")
                            [ ( ( 'a', 'c' ), "def" ) ]
            ]
        ]



-- HELPERS


last : List a -> Maybe a
last =
    List.head << List.reverse


takeHeadAndLast : List a -> Parser ( a, a )
takeHeadAndLast xs =
    case (List.head xs) of
        Nothing ->
            failure

        Just l ->
            case (last xs) of
                Nothing ->
                    failure

                Just r ->
                    return ( l, r )
