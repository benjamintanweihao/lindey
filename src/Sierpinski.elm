module Sierpinski exposing (..)

import Html exposing (..)
import Html.Attributes exposing (rows, style)
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


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { generation = 0 }



-- UPDATE


type Msg
    = NoOp
    | KeyMsg Keyboard.KeyCode



-- VIEW


view : Model -> Html Msg
view model =
    let
        algaeText =
            String.join "" <| List.map toText <| (sierpinski axiom model.generation)
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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.downs KeyMsg ]



-- Sierpinski


type Alphabet
    = SymF
    | SymG
    | SymPos
    | SymNeg


type alias Generation =
    Int


axiom : List Alphabet
axiom =
    [ SymF, SymNeg, SymG, SymNeg, SymG ]


rule : Alphabet -> List Alphabet
rule variable =
    case variable of
        SymF ->
            [ SymF, SymNeg, SymG, SymPos, SymG, SymNeg, SymF ]

        SymG ->
            [ SymG, SymG ]

        _ ->
            []


sierpinski : List Alphabet -> Int -> List Alphabet
sierpinski state generation =
    if generation == 0 then
        state
    else
        sierpinski (List.concatMap rule state) (generation - 1)


toText : Alphabet -> String
toText alphabet =
    case alphabet of
        SymF ->
            "F"

        SymG ->
            "G"

        SymPos ->
            "+"

        SymNeg ->
            "-"
