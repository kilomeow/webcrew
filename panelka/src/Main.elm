module Main exposing (house, main)

import Array exposing (Array, get, set)
import Browser
import Collage exposing (..)
import Collage.Events exposing (..)
import Collage.Layout exposing (center, horizontal, spacer, stack, vertical)
import Collage.Render exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (align, style)
import List exposing (drop, take)



-- Model -----------------------------------------------------------------------


type Window
    = Dark
    | Light
    | Seedlings


houseWidth =
    3


houseHeight =
    4


windowsCount =
    houseWidth * houseHeight


type alias Model =
    Array Window


init : Model
init =
    Array.repeat windowsCount Dark



-- Update ----------------------------------------------------------------------


type alias Msg =
    Int


update : Msg -> Model -> Model
update msg model =
    let
        clickWindow =
            get msg model
    in
    case clickWindow of
        Just Dark ->
            set msg Light model
            
        Just Light ->
            set msg Seedlings model

        Just Seedlings ->
            set msg Dark model

        Nothing ->
            model  -- can this be done better?
            
            
-- View ------------------------------------------------------------------------


house : Model -> Collage Msg
house model =
    let
        split : Int -> List a -> List (List a)
        split n list =
            case take n list of
                [] ->
                    []

                listHead ->
                    listHead :: split n (drop n list)

        fillWindow : Int -> Window -> Collage Msg
        fillWindow id window =
            let
                drawWindowColoredIn fillStyle =
                    filled fillStyle (Collage.square 0.5)
                        |> onClick id
            in
            case window of
                Dark ->
                    drawWindowColoredIn (uniform darkCharcoal)

                Light ->
                    drawWindowColoredIn (uniform lightYellow)

                Seedlings ->
                    drawWindowColoredIn (uniform lightPurple)

        wall =
            rectangle houseWidth houseHeight
                |> filled (uniform charcoal)

        windows =
            List.indexedMap fillWindow (Array.toList model)

        windowsByFloors =
            split houseWidth windows

        makeFloorLayout floor =
            horizontal <| List.intersperse hspace floor

        floors =
            List.map makeFloorLayout windowsByFloors

        hspace =
            spacer 0.3 0

        vspace =
            spacer 0 0.3
    in
    vertical
        [ [ spacer 0 0.1
          , stack
                [ List.intersperse vspace floors
                    |> vertical
                    |> center
                , wall
                ]
          ]
            |> vertical
            |> scale 130
        ]


view : Model -> Html Msg
view model =
    div [ align "center" ]
        [ div [ style "font-size" "2em" ] [ text "Panelka" ]
        , div []
            [ house model
                |> svg
            ]
        ]



-- Main ------------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
