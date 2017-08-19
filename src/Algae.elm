module Algae exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, rows)
import Keyboard


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { generation : Int }


initialModel : Model
initialModel =
    { generation = 0 }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = KeyMsg Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            case code of
                37 ->
                    ( { model
                        | generation = max 0 (model.generation - 1)
                      }
                    , Cmd.none
                    )

                39 ->
                    ( { model
                        | generation = min (model.generation + 1) 10
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.log (toString code)
                        ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.downs KeyMsg ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        algaeText =
            String.join "" <| List.map toText <| (algae axiom model.generation)
    in
        div []
            [ textarea
                [ style
                    [ ( "width", "100%" )
                    ]
                , rows 100
                ]
                [ text algaeText ]
            , text <| "Generation: " ++ (toString model.generation)
            ]



-- Algae


type Alphabet
    = SymA
    | SymB


type alias Generation =
    Int


axiom : List Alphabet
axiom =
    [ SymA ]


rule : Alphabet -> List Alphabet
rule alphabet =
    case alphabet of
        SymA ->
            [ SymA, SymB, SymA ]

        SymB ->
            [ SymB, SymB, SymB ]


algae : List Alphabet -> Int -> List Alphabet
algae state generation =
    if generation == 0 then
        state
    else
        algae (List.concatMap rule state) (generation - 1)


toText : Alphabet -> String
toText alphabet =
    case alphabet of
        SymA ->
            "A"

        SymB ->
            "B"
