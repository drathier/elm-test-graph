module Test.Graph exposing
    ( fuzzGraph
    , Action(..), ExecutionGraph
    )

{-| Execution graphs for [elm-test](/packages/elm-community/elm-test/latest). Define a graph of what operations happen and roughly in what order, and we'll generate total orderings from your graph, and execute them. Hopefully, we'll find some errors this way.


# Fuzzers

@docs fuzzGraph


# Types

@docs Action, ExecutionGraph

-}

import Expect
import Fuzz
import Graph exposing (Graph, getData)
import Graph.Random
import Test


{-| An `Action` to be performed at this step in the graph. `Expect` is used to run an [elm-test](/packages/elm-community/elm-test/latest) expectation against `a` at this point in the execution. `Modify` is used to modify `a` in some way.
-}
type Action a
    = Expect (a -> Expect.Expectation)
    | Modify (a -> a)


{-| A graph of `Action`s to be taken and the partial order to take them in. See [drathier/elm-graph](/packages/drathier/elm-graph/latest) for details on modifying the graph.
-}
type alias ExecutionGraph comparable a =
    Graph comparable (Action a) ()


type alias ExecutionOrdering comparable =
    List comparable


{-| A fuzz test that uses a concurrent execution graph to define what it should do, and then randomizes execution order through the graph.

For example, consider this execution graph:

                Set.insert 1 → Set.remove 1
              ↗                             ↘
    Set.empty → Set.insert 2 → Set.remove 2 → Set.isEmpty
              ↘                             ↗
                Set.insert 3 → Set.remove 3

We could, for example, execute it in this order:

    empty
        |> insert 1
        |> insert 2
        |> remove 1
        |> insert 3
        |> remove 3
        |> remove 2
        |> Set.isEmpty

But who in their right mind would think of writing a test-case like that? Good thing we have this tool to find some of [the really obscure bugs](https://xkcd.com/1700).

The graph above can be modeled using this code (which also has a bunch of expectations mid-way to make narrowing in on an error easier, if there is one):

    fuzzGraph "Inserting and deleting set elements can be done in almost any order" Set.empty <|
        (empty
            |> insertData 11 (Modify <| Set.insert 1)
            |> insertData 12 (Expect <| \set -> set |> Set.member 1 |> Expect.equal True)
            |> insertData 13 (Modify <| Set.remove 1)
            |> insertEdge 11 12
            |> insertEdge 12 13
            |> insertData 21 (Modify <| Set.insert 2)
            |> insertData 22 (Expect <| \set -> set |> Set.member 2 |> Expect.equal True)
            |> insertData 23 (Modify <| Set.remove 2)
            |> insertEdge 21 22
            |> insertEdge 22 23
            |> insertData 31 (Modify <| Set.insert 3)
            |> insertData 32 (Expect <| \set -> set |> Set.member 3 |> Expect.equal True)
            |> insertData 33 (Modify <| Set.remove 3)
            |> insertEdge 31 32
            |> insertEdge 32 33
            |> insertEdge 13 100
            |> insertEdge 23 100
            |> insertEdge 33 100
            |> insertData 100 (Expect <| \set -> set |> Set.isEmpty |> Expect.equal True)
        )

-}
fuzzGraph : String -> a -> ExecutionGraph comparable a -> Test.Test
fuzzGraph description a graph =
    Test.fuzz (fuzzer graph) description <|
        execute graph a


execute : ExecutionGraph comparable a -> a -> ExecutionOrdering comparable -> Expect.Expectation
execute graph a keys =
    case keys of
        [] ->
            Expect.pass

        firstKey :: otherKeys ->
            case graph |> getData firstKey of
                Nothing ->
                    execute graph a otherKeys

                Just (Expect func) ->
                    if func a == Expect.pass then
                        execute graph a otherKeys

                    else
                        func a

                Just (Modify func) ->
                    execute graph (func a) otherKeys


fuzzer : ExecutionGraph comparable a -> Fuzz.Fuzzer (ExecutionOrdering comparable)
fuzzer graph =
    Graph.Random.topologicalSortFuzzer graph
