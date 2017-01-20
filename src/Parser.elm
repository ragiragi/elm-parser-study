module Parser exposing (..)

{-| functional parser study with elm-lang
-}

import String exposing (fromList, uncons, cons)
import Lazy
import Char


-- TYPES


type alias Parser a =
    String -> List ( a, String )



-- BASIC PARSERS


{-| The parser item fails if the input is empty,
    and consumes the first character otherwise:
-}
item : Parser Char
item inp =
    case (uncons inp) of
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



--LazyParser f ->
--    Lazy.force (f inp)
-- SEQUENCING


{-| Sequence two parsers
-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen fn p =
    \inp ->
        case (parse p inp) of
            [] ->
                []

            ( a, restInp ) :: _ ->
                parse (fn a) restInp


{-| infix version of andThen
-}
(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) p fn =
    p |> andThen fn


{-| Sequence multiple parsers and return results
-}
do : List (Parser a) -> Parser (List a)
do ps =
    let
        loop parsers acc =
            case parsers of
                [] ->
                    return (List.reverse acc)

                p :: rest ->
                    p >>= (\a -> loop rest (a :: acc))
    in
        loop ps []



-- DERIVED PRIMITIVES


{-| Parsing a character that satisfies a predicate:
-}
sat : (Char -> Bool) -> Parser Char
sat pred =
    item
        >>= (\c ->
                if (pred c) then
                    return c
                else
                    failure
            )


{-| Parsing a digit
-}
digit : Parser Char
digit =
    sat Char.isDigit


{-| Parsing a specific characters
-}
char : Char -> Parser Char
char c =
    sat ((==) c)


{-| Applying a parser zero or more times:
-}
many : Parser a -> Parser (List a)
many p =
    many1 p +++ return []


{-| Applying a parser one or more times
-}
many1 : Parser a -> Parser (List a)
many1 p =
    p
        |> andThen
            (\x ->
                (many p) >>= (\xs -> return (x :: xs))
            )


{-| Parsing a specific string of characters:
-}
string : String -> Parser String
string str =
    if String.isEmpty str then
        return ""
    else
        case (uncons str) of
            Nothing ->
                failure

            Just ( head, tail ) ->
                char head >>= (\_ -> string tail >>= (\_ -> return str))


lower : Parser Char
lower =
    sat Char.isLower


isAlphanum : Char -> Bool
isAlphanum c =
    Char.isLower c || Char.isUpper c || Char.isDigit c


alphanum : Parser Char
alphanum =
    sat isAlphanum


isSpace : Char -> Bool
isSpace =
    (==) ' '


{-| Parsing an identifier
-}
ident : Parser String
ident =
    lower >>= \c -> (many alphanum) >>= \cs -> return (fromList (c :: cs))


{-| Parsing an natural number
-}
nat : Parser Int
nat =
    many1 digit
        >>= \cs ->
                case String.toInt (fromList cs) of
                    Err _ ->
                        failure

                    Ok n ->
                        return n


{-| Parsing spaces
-}
space : Parser ()
space =
    many (sat isSpace) >>= \_ -> return ()


{-| Make token parser
-}
token : Parser a -> Parser a
token p =
    space >>= \_ -> p >>= \v -> space >>= \_ -> return v


identifier : Parser String
identifier =
    token ident


natural : Parser Int
natural =
    token nat


symbol : String -> Parser String
symbol s =
    token (string s)


{-| example parser to parse "[1,2,3,4...,n] pattern"
-}
exP : Parser String
exP =
    let
        openingBracket =
            char '['

        closingBracket =
            char ']'

        digitToken =
            token digit

        followingDigits =
            many (char ',' >>= (\_ -> digitToken))
    in
        openingBracket
            >>= (\_ ->
                    digitToken
                        >>= (\d ->
                                followingDigits
                                    >>= (\ds ->
                                            closingBracket
                                                >>= (\_ ->
                                                        return
                                                            (fromList (d :: ds))
                                                    )
                                        )
                            )
                )



{-
   expr ::= term (+expr | empty)
   term ::= factor (*term | emtpy)
   factor ::= (expr) | nat
   nat ::= 0 | 1 | 2 | ...
-}
{-
   elm 은 haskell 과 다르게 lazy evalutation 을 언어 차원에서 지원하지 않기 때문에
   elm-lang/lazy 를 이용하여 lazy evalutation 을 구현해주어야 한다.

   위의 구현 내용들은 제외하고 실제 recursion 이 필요한 아래 산술 연산 예제만
   lazy eval 을 사용하도록 구현하였다.
-}


type P a
    = EagerParser (Parser a)
    | LazyParser (Lazy.Lazy (Parser a))


lazy : (() -> P a) -> P a
lazy t =
    LazyParser (Lazy.lazy (\() -> parse_ (t ())))


parse_ : P a -> String -> List ( a, String )
parse_ p =
    case p of
        EagerParser p_ ->
            p_

        LazyParser t ->
            Lazy.force t


symbol_ : String -> P String
symbol_ str =
    EagerParser (symbol str)


return_ : a -> P a
return_ v =
    EagerParser (return v)


natural_ : P Int
natural_ =
    EagerParser natural


{-| lazy version >>=
-}
(>>>=) : P a -> (a -> P b) -> P b
(>>>=) p fn =
    EagerParser <|
        \inp ->
            case (parse_ p inp) of
                [] ->
                    []

                ( a, restInp ) :: _ ->
                    parse_ (fn a) restInp


{-| lazy version +++
-}
(++++) : P a -> P a -> P a
(++++) p q =
    EagerParser <|
        \inp ->
            let
                resP =
                    parse_ p inp
            in
                case resP of
                    [] ->
                        parse_ q inp

                    _ ->
                        resP


factor : P Int
factor =
    (symbol_ "(" >>>= \_ -> expr >>>= \e -> symbol_ ")" >>>= \_ -> return_ e)
        ++++ natural_


term : P Int
term =
    lazy <|
        \() ->
            factor
                >>>=
                    \f ->
                        (symbol_ "*" >>>= \_ -> term >>>= \t -> return_ (f * t))
                            ++++ return_ f


expr : P Int
expr =
    lazy <|
        \() ->
            term
                >>>=
                    \t ->
                        (symbol_ "+" >>>= \_ -> expr >>>= \e -> return_ (t + e))
                            ++++ return_ t
