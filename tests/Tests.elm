module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Chain exposing (Action(Assert, Modify))
import Shrink


all : Test
all =
  describe "Sample Test Suite"
    [ describe "Unit test examples"
        [ let
            plan =
              [ [ Assert (always Expect.pass) ], [ Modify identity, Modify identity ] ]
          in
            fuzz (Fuzz.custom (Chain.interleave plan) Shrink.noShrink) "Chain plan works" <|
              \order ->
                order |> Debug.log "order" |> always Expect.pass
        ]
    ]
