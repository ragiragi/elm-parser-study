module ThunkNat exposing (..)

import Trampoline exposing (..)


type Nat
    = Z
    | S (Thunk Nat)


type alias Thunk a =
    () -> a


force : Thunk a -> a
force thunk =
    thunk ()


delay : a -> Thunk a
delay e =
    \() -> e


fromInt : Int -> Nat
fromInt n =
    if n <= 0 then
        Z
    else
        S (\() -> fromInt (n - 1))


toInt : Nat -> Int
toInt n =
    let
        foo acc n =
            case n of
                Z ->
                    done acc

                S prevThunk ->
                    jump (\_ -> foo (1 + acc) (force prevThunk))
    in
        evaluate (foo 0 n)
