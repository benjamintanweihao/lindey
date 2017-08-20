module Sierpinski exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (readonly, rows, style)
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
        rendered =
            String.join "" <| List.map toText <| sierpinski axiom model.generation
    in
    div []
        [ canvas model.generation |> toHtml
        , textarea
            [ style
                [ ( "width", "100%" )
                ]
            , rows 30
            , readonly True
            ]
            [ Html.text rendered ]
        , Html.text <| "Generation: " ++ toString model.generation
        ]


segmentLength : Float
segmentLength =
    4


canvas : Int -> Element
canvas generation =
    collage 800 800 [ drawPoints generation ]


drawPoints : Int -> Form
drawPoints generation =
    traced defaultLine
        (path <| computePoints generation)


computePoints : Int -> List ( Float, Float )
computePoints generation =
    let
        coordsReversed =
            List.foldr
                (\alphabet acc ->
                    let
                        newCoord =
                            symbolToInstruction alphabet (List.head acc |> Maybe.withDefault ( 0, 0 ))
                    in
                    newCoord :: acc
                )
                [ ( 0, 0 ) ]
                (sierpinski axiom generation)
    in
    Debug.log "Coords"
        (List.reverse
            coordsReversed
        )


symbolToInstruction : Alphabet -> ( Float, Float ) -> ( Float, Float )
symbolToInstruction alphabet =
    case alphabet of
        SymF ->
            Debug.log "drawForward"
                drawForward

        SymG ->
            Debug.log "drawForward"
                drawForward

        SymPos ->
            Debug.log "rotateLeft"
                rotateLeft

        SymNeg ->
            Debug.log "rotateRight"
                rotateRight


drawForward : ( Float, Float ) -> ( Float, Float )
drawForward ( x, y ) =
    ( x + segmentLength, y )


rotateLeft : ( Float, Float ) -> ( Float, Float )
rotateLeft ( x, y ) =
    let
        newX =
            -0.5 * x - (sqrt 3 / 2) * y

        newY =
            -0.5 * y + (sqrt 3 / 2) * x
    in
    ( newX, newY )


rotateRight : ( Float, Float ) -> ( Float, Float )
rotateRight ( x, y ) =
    let
        newX =
            -0.5 * x + (sqrt 3 / 2) * y

        newY =
            -0.5 * y - (sqrt 3 / 2) * x
    in
    ( newX, newY )



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
            [ SymF, SymNeg, SymG, SymPos, SymF, SymPos, SymG, SymNeg, SymF ]

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
