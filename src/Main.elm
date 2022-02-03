module Main exposing (..)

import Browser
import Dict
import Element exposing (Element, column, el, padding, paddingEach, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra as Maybe2


type alias Model =
    { puzzle : String
    , hovered : Maybe Char
    , selected : Maybe Char
    }


type Msg
    = MouseEnter Char
    | MouseLeave Char
    | Click Char


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { puzzle = "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."
            , hovered = Nothing
            , selected = Nothing
            }
        , update = update
        , view = view
        }


isnt : a -> a -> Bool
isnt x y =
    x /= y


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click c ->
            applyClick model c

        MouseEnter c ->
            { model | hovered = Just c }

        MouseLeave c ->
            { model | hovered = model.hovered |> Maybe2.filter (isnt c) }


applyClick : Model -> Char -> Model
applyClick model c =
    case model.selected of
        Nothing ->
            { model | selected = Just c }

        Just s ->
            { model | puzzle = String.map (charSwap c s) model.puzzle, selected = Nothing }


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


view : Model -> Html Msg
view model =
    [ viewPuzzle model ]
        |> column
            [ Element.centerX
            , Element.centerY
            ]
        |> Element.layout
            [ padding 10
            ]


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
        -- |> List.intersperse (el [ charPadding ] <| text " ")
        |> wrappedRow [ style "max-width" "60em" ]
        |> el
            [ style "padding-left" "10em"
            , style "padding-right" "10em"
            , Font.family [ Font.monospace ]
            ]


viewWord : { a | hovered : Maybe Char, selected : Maybe Char } -> String -> Element Msg
viewWord model word =
    word
        |> String.toList
        |> List.map (viewCharacter model)
        |> row []


viewCharacter : { a | hovered : Maybe Char, selected : Maybe Char } -> Char -> Element Msg
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

        extras =
            if Char.isAlpha c then
                [ onMouseEnter (MouseEnter cLower)
                , onMouseLeave (MouseLeave cLower)
                , onClick (Click cLower)
                , cursor cursorName
                , style "touch-action" "manipulation"
                ]

            else
                []
    in
    String.fromChar c
        |> text
        |> el
            ([ charPadding
             , Border.rounded 5
             , Background.color bgColor
             ]
                ++ extras
            )


charPadding =
    padding 5


cursor : String -> Element.Attribute msg
cursor name =
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
    style "cursor" name


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute <| Html.Attributes.style name value
