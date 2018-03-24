module Tests exposing (..)

import Graph exposing (empty, insertEdge, insertData)
import Set
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Test.Graph exposing (Action(Expect, Modify), fuzzGraph)
import Shrink


all : Test
all =
  describe "Sample Test Suite"
    [ describe "Unit test examples"
        [ fuzzGraph "Simple execution graph" 42 <|
            (empty
              |> insertData 1 (Modify identity)
              |> insertData 2 (Expect (always Expect.pass))
              |> insertData 3 (Modify identity)
              |> insertEdge 1 2
              |> insertEdge 1 3
            )
        , fuzzGraph "Not all nodes need to have an Action" 42 <|
            (empty
              |> insertData 1 (Modify identity)
              |> insertData 3 (Modify identity)
              |> insertEdge 1 2
              |> insertEdge 1 3
            )
        , fuzzGraph "Addition and Subtraction can be done in any order" 42 <|
            (empty
              |> insertData 21 (Modify ((+) 2))
              |> insertData 22 (Modify ((+) -2))
              |> insertData 31 (Modify ((+) 3))
              |> insertData 32 (Modify ((+) -3))
              |> insertData 51 (Modify ((+) 5))
              |> insertData 52 (Modify ((+) -5))
              |> insertData 100 (Expect (Expect.equal 42))
              |> insertEdge 21 100
              |> insertEdge 22 100
              |> insertEdge 31 100
              |> insertEdge 32 100
              |> insertEdge 51 100
              |> insertEdge 52 100
            )
        , fuzzGraph "Inserting and deleting set elements can be done in almost any order" Set.empty <|
            (empty
              |> insertData 11 (Modify <| Set.insert 1)
              |> insertData 12 (Expect <| (\set -> set |> Set.member 1 |> Expect.true "1 should be a member"))
              |> insertData 13 (Modify <| Set.remove 1)
              |> insertEdge 11 12
              |> insertEdge 12 13
              |> insertData 21 (Modify <| Set.insert 2)
              |> insertData 22 (Expect <| (\set -> set |> Set.member 2 |> Expect.true "2 should be a member"))
              |> insertData 23 (Modify <| Set.remove 2)
              |> insertEdge 21 22
              |> insertEdge 22 23
              |> insertData 31 (Modify <| Set.insert 3)
              |> insertData 32 (Expect <| (\set -> set |> Set.member 3 |> Expect.true "3 should be a member"))
              |> insertData 33 (Modify <| Set.remove 3)
              |> insertEdge 31 32
              |> insertEdge 32 33
              |> insertEdge 13 100
              |> insertEdge 23 100
              |> insertEdge 33 100
              |> insertData 100 (Expect (\set -> set |> Set.isEmpty |> Expect.true "expected set to be empty"))
            )
        ]
    ]
