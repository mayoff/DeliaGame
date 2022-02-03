module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict
import Element exposing (Element, column, el, padding, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra as Maybe2
import Random.Pcg.Extended as Pcg
import Set


type alias Model =
    { puzzle : String
    , solution : String
    , history : List String
    , hovered : Maybe Char
    , selected : Maybe Char
    , hasMouse : Bool
    }


type Msg
    = MouseEnter Char
    | MouseLeave Char
    | Click Char
    | Undo


main : Program AppFlags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias AppFlags =
    { hasMouse : Bool
    , randomSeed : List Int
    }


init : AppFlags -> ( Model, Cmd Msg )
init flags =
    let
        solution =
            "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."

        seed =
            Pcg.initialSeed
                (List.head flags.randomSeed |> Maybe.withDefault 123)
                (List.tail flags.randomSeed |> Maybe.withDefault [ 456 ])
    in
    ( { puzzle = shuffle solution seed
      , solution = solution
      , history = []
      , hovered = Nothing
      , selected = Nothing
      , hasMouse = flags.hasMouse
      }
    , Cmd.none
    )


mapSecond : (a -> b) -> ( c, a ) -> ( c, b )
mapSecond f ( c, a ) =
    ( c, f a )


shuffle : String -> Pcg.Seed -> String
shuffle string initialSeed =
    let
        pick : Int -> List Char -> Maybe ( Char, List Char )
        pick i cs =
            case cs of
                [] ->
                    Nothing

                c :: rest ->
                    if i == 0 then
                        Just ( c, rest )

                    else
                        pick (i - 1) rest |> Maybe.map (mapSecond (\rest1 -> c :: rest1))

        pickRandom : Pcg.Seed -> Int -> List Char -> Maybe ( Char, List Char, Pcg.Seed )
        pickRandom seed cCount cs =
            if cCount == 0 then
                Nothing

            else
                let
                    gen =
                        Pcg.int 0 (cCount - 1)

                    ( i, seed1 ) =
                        Pcg.step gen seed
                in
                case pick i cs of
                    Nothing ->
                        Nothing

                    Just ( c, rest ) ->
                        Just ( c, rest, seed1 )

        shuffleChars : Pcg.Seed -> Int -> List Char -> List Char
        shuffleChars seed cCount cs =
            case pickRandom seed cCount cs of
                Nothing ->
                    []

                Just ( c, rest, seed1 ) ->
                    c :: shuffleChars seed1 (cCount - 1) rest

        uniqueChars =
            string |> String.toList |> List.map Char.toLower |> List.filter Char.isAlpha |> Set.fromList |> Set.toList

        shuffledChars =
            shuffleChars initialSeed (List.length uniqueChars) uniqueChars

        withUppers =
            List.concatMap (\c -> [ c, Char.toUpper c ])

        translationTable =
            List.map2 (\a b -> ( a, b )) (withUppers uniqueChars) (withUppers shuffledChars) |> Dict.fromList

        translate c =
            Dict.get c translationTable |> Maybe.withDefault c
    in
    String.map translate string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click c ->
            ( applyClick model c, Cmd.none )

        MouseEnter c ->
            ( { model | hovered = Just c }, Cmd.none )

        MouseLeave c ->
            let
                isnt x y =
                    x /= y
            in
            ( { model | hovered = model.hovered |> Maybe2.filter (isnt c) }, Cmd.none )

        Undo ->
            ( applyUndo model, Cmd.none )


applyClick : Model -> Char -> Model
applyClick model c =
    case model.selected of
        Nothing ->
            { model | selected = Just c }

        Just s ->
            let
                newPuzzle =
                    String.map (charSwap c s) model.puzzle
            in
            setPuzzle { model | selected = Nothing } newPuzzle


setPuzzle : Model -> String -> Model
setPuzzle model puzzle =
    if puzzle == model.puzzle then
        model

    else
        { model | puzzle = puzzle, history = model.puzzle :: model.history }


charSwap : Char -> Char -> Char -> Char
charSwap x y z =
    let
        table =
            Dict.fromList
                [ ( Char.toUpper x, Char.toUpper y )
                , ( Char.toLower x, Char.toLower y )
                , ( Char.toUpper y, Char.toUpper x )
                , ( Char.toLower y, Char.toLower x )
                ]
    in
    Dict.get z table |> Maybe.withDefault z


applyUndo model =
    case model.history of
        first :: rest ->
            { model | puzzle = first, history = rest }

        [] ->
            model


view : Model -> Html Msg
view model =
    [ viewPuzzle model
    , undoButton model
    ]
        ++ instructions model
        |> column
            [ Element.centerX
            , Element.centerY
            , Element.spacing 30
            ]
        |> Element.layout
            [ style "padding" "3em"
            ]


instructions : Model -> List (Element Msg)
instructions model =
    [ [ text """
          Rearrange the letters to make a well-known phrase.
          """ ] |> Element.paragraph []
    , [ String.concat
            [ titlize <| clickVerb model
            , " two letters to swap them. For example, "
            , clickVerb model
            , " ‘t’ and ‘k’ to replace every ‘t’ with a ‘k’ and every ‘k’ with a ‘t‘."
            ]
            |> text
      ]
        |> Element.paragraph []
    ]


clickVerb : Model -> String
clickVerb model =
    if model.hasMouse then
        "click"

    else
        "tap"


titlize : String -> String
titlize s =
    String.append (s |> String.left 1 |> String.toUpper) (s |> String.dropLeft 1)


undoButton : Model -> Element Msg
undoButton model =
    Html.button
        [ Html.Events.onClick Undo
        , Html.Attributes.disabled <| List.isEmpty model.history
        , Html.Attributes.style "font-size" "24px"
        ]
        [ Html.text "Undo"
        ]
        |> Element.html
        |> el [ Element.centerX ]


viewPuzzle model =
    let
        words =
            model.puzzle
                |> String.words
                |> List.foldr
                    (\word list ->
                        if List.isEmpty list then
                            [ word ]

                        else
                            String.append word " " :: list
                    )
                    []
    in
    words
        |> List.map (viewWord model)
        |> wrappedRow
            [ Element.centerX
            , Font.family [ Font.monospace ]
            , Font.size 34
            ]


viewWord : { a | hasMouse : Bool, hovered : Maybe Char, selected : Maybe Char } -> String -> Element Msg
viewWord model word =
    word
        |> String.toList
        |> List.map (viewCharacter model)
        |> row []


viewCharacter : { a | hasMouse : Bool, hovered : Maybe Char, selected : Maybe Char } -> Char -> Element Msg
viewCharacter model c =
    let
        cLower =
            Char.toLower c

        bgColor =
            if model.selected == Just cLower then
                Element.rgb 1 0.6 0.6

            else if model.hovered == Just cLower then
                Element.rgb 0.6 1 0.6

            else
                Element.rgba 0 0 0 0

        cursorName =
            case model.selected of
                Just _ ->
                    "grabbing"

                Nothing ->
                    "grab"

        hoverEvents =
            if model.hasMouse then
                [ onMouseEnter (MouseEnter cLower)
                , onMouseLeave (MouseLeave cLower)
                , cursor cursorName
                ]

            else
                []

        extras =
            if Char.isAlpha c then
                hoverEvents
                    ++ [ onClick (Click cLower)
                       , style "touch-action" "manipulation"
                       ]

            else
                []
    in
    String.fromChar c
        |> text
        |> el
            ([ padding 5
             , Border.rounded 5
             , Background.color bgColor
             ]
                ++ extras
            )


cursor : String -> Element.Attribute msg
cursor name =
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
    style "cursor" name


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute <| Html.Attributes.style name value
