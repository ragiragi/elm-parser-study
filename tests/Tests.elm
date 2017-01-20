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
        , describe "sat"
            [ test "return a char if it is satisfied" <|
                \() ->
                    let
                        p1 =
                            sat (Char.isUpper)
                    in
                        Expect.equal (parse p1 "Hello") [ ( 'H', "ello" ) ]
            , test "be failed it it is not" <|
                \() ->
                    Expect.equal (parse (sat Char.isUpper) "hello") []
            ]
        , describe "digit"
            [ test "get a digit char" <|
                \() ->
                    Expect.equal (parse digit "123") [ ( '1', "23" ) ]
            , test "fail if char is not a digit" <|
                \() ->
                    Expect.equal (parse digit "a23") []
            ]
        , describe "char"
            [ test "get a 'a' char" <|
                \() ->
                    Expect.equal (parse (char 'a') "abc") [ ( 'a', "bc" ) ]
            , test "fail if char is not match" <|
                \() ->
                    Expect.equal (parse (char 'Z') "abc") []
            ]
        , describe "many"
            [ test "try to get digit, returns empty list" <|
                \() ->
                    Expect.equal
                        (parse (many digit) "abc")
                        [ ( [], "abc" ) ]
            , test "get many digits" <|
                \() ->
                    Expect.equal
                        (parse (many digit) "12abc")
                        [ ( [ '1', '2' ], "abc" ) ]
            ]
        , describe "many1"
            [ test "get one or more digit" <|
                \() ->
                    Expect.equal
                        (parse (many1 digit) "12abc")
                        [ ( [ '1', '2' ], "abc" ) ]
            , test "fail if there is no digit" <|
                \() ->
                    Expect.equal
                        (parse (many1 digit) "abc")
                        []
            ]
        , describe "string"
            [ test "match a string" <|
                \() ->
                    Expect.equal (parse (string "abc") "abcdef")
                        [ ( "abc", "def" ) ]
            , test "fail if not match" <|
                \() ->
                    Expect.equal (parse (string "abc") "Hello") []
            ]
        , describe "ident"
            [ test "match a identifier" <|
                \() ->
                    Expect.equal (parse ident "foo1") [ ( "foo1", "" ) ]
            ]
        , describe "nat"
            [ test "match a natural number" <|
                \() -> Expect.equal (parse nat "345+") [ ( 345, "+" ) ]
            ]
        , describe "space"
            [ test "match leading spaces" <|
                \() -> Expect.equal (parse space "   hi") [ ( (), "hi" ) ]
            ]
        , describe "token parser"
            [ test "number token parser" <|
                \() -> Expect.equal (parse (token nat) "  123 +") [ ( 123, "+" ) ]
            , test "symbol parser" <|
                \() ->
                    Expect.equal
                        (parse (symbol "if") "if True then")
                        [ ( "if", "True then" ) ]
            ]
        , describe "example digit list parser"
            [ test "match [ 1,2 , 3, 4 ]" <|
                \() ->
                    Expect.equal (parse exP "[ 1,2 , 3, 4 ]") [ ( "1234", "" ) ]
            , test "missing closing bracket" <|
                \() ->
                    Expect.equal (parse exP "[1,2,3,4") []
            ]
        , describe "parsing artithmatic expression"
            [ describe "factor"
                [ test "number with bracket" <|
                    \() -> Expect.equal (app factor "(123)") [ ( 123, "" ) ]
                , test "just number" <|
                    \() -> Expect.equal (app factor "123") [ ( 123, "" ) ]
                ]
            , describe "term"
                [ test "multiply two number" <|
                    \() -> Expect.equal (app term "5 * 3") [ ( 15, "" ) ]
                ]
            , describe "expr"
                [ test "add two number" <|
                    \() -> Expect.equal (app expr "5 + 3") [ ( 8, "" ) ]
                ]
            , test "2 * 3 + 4" <|
                \() -> Expect.equal (app expr "2 * 3 + 4") [ ( 10, "" ) ]
            , test "2 + 3 * 4" <|
                \() -> Expect.equal (app expr "2 + 3 * 4") [ ( 14, "" ) ]
            , test "2 * (3 + 4)" <|
                \() -> Expect.equal (app expr "2 * (3) + 4") [ ( 10, "" ) ]
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
