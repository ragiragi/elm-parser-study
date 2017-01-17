module Tests exposing (..)

import Test exposing (..)
import Parser exposing (..)
import Expect


--import Fuzz exposing (list, int, tuple, string)
--import String


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
        ]



{-
   [ describe "Unit test examples"
       [ test "Addition" <|
           \() ->
               Expect.equal (3 + 7) 10
       , test "String.left" <|
           \() ->
               Expect.equal "a" (String.left 1 "abcdefg")
       , test "This test should fail - you should remove it" <|
           \() ->
               Expect.fail "Failed as expected!"
       ]
   , describe "Fuzz test examples, using randomly generated input"
       [ fuzz (list int) "Lists always have positive length" <|
           \aList ->
               List.length aList |> Expect.atLeast 0
       , fuzz (list int) "Sorting a list does not change its length" <|
           \aList ->
               List.sort aList |> List.length |> Expect.equal (List.length aList)
       , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
           \i ->
               List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
       , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
           \s1 s2 ->
               s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
       ]
   ]
-}
