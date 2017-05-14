module Chain exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import Random.List
import Random.Pcg as Random


type Action a
  = Assert (a -> Expect.Expectation)
  | Modify (a -> a)


type alias Plan a =
  List (List (Action a))


{- execute : Plan a -> Fuzzer a
   execute plan =
     let
       generator =
         3

       shrinker =
         2
     in
       Fuzz.custom generator shrinker
-}


{-| Get nth element of the list. If the list is empty, the selected element
   will be `Nothing`.
-}
get : Int -> List a -> Maybe a
get index list =
  list
    |> List.drop index
    |> List.head


{-| Sample without replacement: produce a randomly selected element of the
list, and the list with that element omitted. If the list is empty, the
selected element will be `Nothing`.
-}
choose : List a -> Random.Generator ( Maybe a, List a )
choose list =
  if List.isEmpty list then
    Random.constant ( Nothing, list )
  else
    let
      lastIndex =
        List.length list - 1

      front i =
        List.take i list

      back i =
        List.drop (i + 1) list

      gen =
        Random.int 0 lastIndex
    in
      Random.map
        (\index ->
          ( get index list, List.append (front index) (back index) )
        )
        gen


{-| Create a generator that chooses a generator from a list of generators
based on the provided weight. The likelihood of a given generator being
chosen is its weight divided by the total weight (which doesn't have to equal 1).

**Warning:** Do not pass an empty list or your program will crash! In practice
this is usually not a problem since you pass a list literal.
-}
frequency : List ( Float, a ) -> Random.Generator ( a, List a )
frequency pairs =
  let
    total =
      List.sum <| List.map (Tuple.first >> abs) pairs

    lastIndex =
      List.length pairs - 1

    front i =
      List.take i pairs

    back i =
      List.drop (i + 1) pairs

    gen =
      Random.int 0 lastIndex

    pick : List ( Float, a ) -> Int -> Float -> ( a, List a )
    pick choices pos n =
      case choices of
        ( k, g ) :: rest ->
          if n <= k then
            ( g, List.append (front pos) (back pos) |> List.map Tuple.second )
          else
            pick rest (pos + 1) (n - k)

        _ ->
          Debug.crash "Empty list passed to Random.Pcg.frequency!"
  in
    Random.float 0 total |> Random.map (pick pairs 0)


{-| Interleave a list of actions into a list of pairs describing where the operation came from, and what operation it is.
-}
interleave : Plan a -> Random.Generator (List (Action a))
interleave plan =
  -- take the first value off of a randomly chosen list, then recurse until all lists are empty
  if plan == [ [] ] then
    Random.constant []
  else
    plan
      |> List.map (\a -> ( List.length a |> toFloat, a ))
      |> frequency
      |> Random.andThen
          (\( elem, restPlan ) ->
            case elem of
              x :: xs ->
                interleave (xs :: restPlan) |> Random.map ((::) x)

              [] ->
                interleave restPlan
          )
