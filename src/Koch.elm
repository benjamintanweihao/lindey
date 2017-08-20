module Koch exposing (..)

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
        alphabets =
            koch axiom model.generation

        rendered =
            String.join "" <| List.map toText <| alphabets
    in
        div []
            [ canvas alphabets |> toHtml
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
    20


canvas : List Alphabet -> Element
canvas alphabets =
    collage 1000 1000 [ eval alphabets ]


initPos : ( Float, Float )
initPos =
    ( 0, 0 )


initRotation : Float
initRotation =
    0


eval : List Alphabet -> Form
eval alphabets =
    let
        angle =
            degrees 90

        ( _, _, paths ) =
            List.foldl
                (\alphabet ( pos, r, paths ) ->
                    case alphabet of
                        SymF ->
                            let
                                endPos =
                                    calcEndPos pos r

                                newPath =
                                    traced (solid black) <| segment pos endPos
                            in
                                ( endPos, r, newPath :: paths )

                        SymPos ->
                            ( pos, r + angle, paths )

                        SymNeg ->
                            ( pos, r - angle, paths )
                )
                ( initPos, initRotation, [] )
                alphabets
    in
        paths |> group


calcEndPos : ( Float, Float ) -> Float -> ( Float, Float )
calcEndPos ( x, y ) rotation =
    let
        segmentLength_ =
            toFloat (round segmentLength)

        endX =
            x + (segmentLength_ * cos rotation)

        endY =
            y + (segmentLength_ * sin rotation)
    in
        ( endX, endY )



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



-- Koch


type Alphabet
    = SymF
    | SymPos
    | SymNeg


type alias Generation =
    Int


koch : List Alphabet -> Int -> List Alphabet
koch state generation =
    if generation == 0 then
        state
    else
        koch (List.concatMap rule state) (generation - 1)


axiom : List Alphabet
axiom =
    [ SymF ]


rule : Alphabet -> List Alphabet
rule variable =
    case variable of
        SymF ->
            [ SymF, SymPos, SymF, SymNeg, SymF, SymNeg, SymF, SymPos, SymF ]

        sym ->
            [ sym ]


toText : Alphabet -> String
toText alphabet =
    case alphabet of
        SymF ->
            "F"

        SymPos ->
            "+"

        SymNeg ->
            "-"
