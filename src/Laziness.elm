module Laziness exposing (..)

import Trampoline exposing (..)


type Nat
    = Z
    | S Nat


fromInt : Int -> Nat
fromInt n =
    let
        foo acc n =
            if n <= 0 then
                done acc
            else
                jump (\_ -> foo (S acc) (n - 1))
    in
        evaluate (foo Z n)


strNat : Nat -> String
strNat n =
    let
        foo acc n =
            case n of
                Z ->
                    done acc

                S prev ->
                    jump (\_ -> foo ("S" ++ acc) prev)
    in
        evaluate (foo "Z" n)


plus : Nat -> Nat -> Nat
plus x y =
    let
        foo acc n =
            case n of
                Z ->
                    done acc

                S prev ->
                    jump (\_ -> foo (S acc) prev)
    in
        evaluate (foo x y)


eqNat : Nat -> Nat -> Bool
eqNat x y =
    let
        foo x y =
            case ( x, y ) of
                ( Z, Z ) ->
                    done True

                ( S xp, S yp ) ->
                    jump (\() -> foo xp yp)

                _ ->
                    done False
    in
        evaluate (foo x y)
