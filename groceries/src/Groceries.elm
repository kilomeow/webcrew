module Groceries exposing (..)

-- Show a list of items I need to buy at the grocery store. --


import Array exposing (Array, get, push, set)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple exposing (first, second)


main =
    Browser.sandbox
        { init = our_groceries
        , update = update
        , view = view
        }



-- Grocery Sandbox --


twoWords : String -> String -> String
twoWords a b =
    a ++ " " ++ b


kitten =
    twoWords "Kitten"


beans a =
    twoWords a "Beans"


our_groceries : Array ( String, Bool )
our_groceries =
    [ kitten "Food"
    , kitten "Toy"
    , "Blue" |> beans
    ]
        |> List.map (\grocery -> ( grocery, False ))
        |> Array.fromList



-- Model --


type alias Model =
    Array ( String, Bool )



-- Update --


type Msg
    = AddCarrot
    | Strike Int


update : Msg -> Model -> Model
update msg items =
    case msg of
        AddCarrot ->
            push ( "Carrot", False ) items

        Strike n ->
            let
                nthElem =
                    get n items
            in case nthElem of
                Just (grocery, isStriked) ->
                    set n ( grocery, not isStriked ) items
               
                Nothing -> 
                    items


-- View --


createList =
    Array.indexedMap
        (\i ->
            \( name, strike ) ->
                li
                    [ style "text-decoration"
                        (if strike then
                            "line-through"

                         else
                            "none"
                        )
                    , onClick (Strike i)
                    ]
                    [ text name ]
        )


view : Model -> Html Msg
view items =
    div
        [ class "content" ]
        [ h1 [] [ text "My Grocery List" ]
        , items
            |> createList
            |> Array.toList
            |> ul [ style "font-size" "18pt" ]
        , button [ onClick AddCarrot ] [ text "Add Carrot!" ]
        ]
