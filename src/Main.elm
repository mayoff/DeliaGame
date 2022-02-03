module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Dict
import Element exposing (Element, column, el, padding, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe exposing (withDefault)
import Maybe.Extra as Maybe2
import Random.Pcg.Extended as Pcg
import Set
import Url


type alias Model =
    { puzzle : String
    , solution : String
    , history : List String
    , hovered : Maybe Char
    , selected : Maybe Char
    , hasMouse : Bool
    }


type Msg
    = Click Char
    | Ignore
    | MouseEnter Char
    | MouseLeave Char
    | Undo


main : Program AppFlags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> Ignore
        , onUrlRequest = \_ -> Ignore
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = document
        }


type alias AppFlags =
    { hasMouse : Bool
    , randomSeed : List Int
    }


init : AppFlags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags _ _ =
    let
        solution =
            --"It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."
            "Well-behaved women seldom make history."

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


{-| Create a puzzle from a solution by swapping letters. This could in theory return the solution as the puzzle.
-}
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


{-| Create a puzzle from a solution by swapping letters. This never returns the solution as the puzzle, but it will hang if the solution contains less than two distinct letters!
-}
guaranteedShuffle : String -> Pcg.Seed -> String
guaranteedShuffle solution seed =
    let
        puzzle =
            shuffle solution seed
    in
    if puzzle /= solution then
        puzzle

    else
        case Pcg.step (Pcg.int Pcg.minInt Pcg.maxInt) seed of
            ( _, seed1 ) ->
                guaranteedShuffle solution seed1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click c ->
            ( applyClick model c, Cmd.none )

        Ignore ->
            ( model, Cmd.none )

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


document : Model -> Document Msg
document model =
    { title = "Delia's Game"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    let
        paragraph s =
            Element.paragraph [] [ text s ]
    in
    [ "Can you unscramble the letters of the following famous quotation?" |> paragraph
    , titlize (clickVerb model) ++ " two letters to swap them. " ++ hint model |> paragraph
    , puzzleView model
    , "If you're having trouble finding a letter in the puzzle above, check the list of letters below. You can also " ++ clickVerb model ++ " those letters!" |> paragraph
    , inventoryView model
    , undoButton model
    ]
        |> column
            [ Element.centerX
            , Element.centerY
            , Element.spacing 30
            ]
        |> Element.layout
            [ style "padding" "3em"
            ]


hint : Model -> String
hint model =
    let
        pairs =
            List.map2 (\a b -> ( a, b )) (model.puzzle |> String.toLower |> String.toList) (model.solution |> String.toLower |> String.toList)

        areNotSame ( a, b ) =
            a /= b

        ( xChar, yChar ) =
            pairs |> List.filter areNotSame |> List.head |> withDefault ( 'x', 'y' )

        quote c =
            "‘" ++ String.fromChar c ++ "’"

        ( x, y, verb ) =
            ( quote xChar, quote yChar, clickVerb model )
    in
    "For example, " ++ verb ++ " " ++ x ++ " and " ++ y ++ " to replace every " ++ x ++ " with " ++ y ++ " and vice versa."


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
        |> el
            [ Element.centerX
            , unselectable
            ]


puzzleView model =
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
        |> List.map (wordView model)
        |> wrappedRow
            [ Element.centerX
            , Font.family [ Font.monospace ]
            , Font.size 34
            , unselectable
            ]


wordView : { a | hasMouse : Bool, hovered : Maybe Char, selected : Maybe Char } -> String -> Element Msg
wordView model word =
    word
        |> String.toList
        |> List.map (charView model)
        |> row [ unselectable ]


inventoryView : Model -> Element Msg
inventoryView model =
    let
        letters =
            model.solution
                |> String.toLower
                |> String.filter Char.isAlpha
                |> String.toLower
                |> String.toList
                |> Set.fromList
                |> Set.toList
                |> List.sort
                |> List.map (charView model)
    in
    wrappedRow
        [ Element.centerX
        , Font.family [ Font.monospace ]
        , Font.size 34
        , unselectable
        ]
    <|
        [ text "Letters: " ]
            ++ letters


charView : { a | hasMouse : Bool, hovered : Maybe Char, selected : Maybe Char } -> Char -> Element Msg
charView model c =
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
             , unselectable
             ]
                ++ extras
            )


cursor : String -> Element.Attribute msg
cursor name =
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
    style "cursor" name


unselectable : Element.Attribute msg
unselectable =
    Element.htmlAttribute <| Html.Attributes.attribute "style" """
      -moz-user-select: none;
      -webkit-user-select: none;
      -ms-user-select: none;
      user-select: none;
    """


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute <| Html.Attributes.style name value
