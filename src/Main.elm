module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, padding, row, text, wrappedRow)
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
import Tuple exposing (mapBoth)
import Url


type alias Quotation =
    { text : String
    , author : String
    }


type alias Model =
    { puzzle : String
    , quotation : Quotation
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
        {-
           { text = "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."
           , author = "Jane Austen"
           }
           { text = "Well-behaved women seldom make history."
           , author = "Laurel Thatcher Ulrich"
           }
            { text = "A woman is like a tea bag; you never know how strong it is until it's in hot water."
            , author = "Eleanor Roosevelt"
            }
        -}
        quotation =
            { text = "A woman without a man is like a fish without a bicycle."
            , author = "Irina Dunn"
            }

        seed0 =
            Pcg.initialSeed
                (List.head flags.randomSeed |> Maybe.withDefault 123)
                (List.tail flags.randomSeed |> Maybe.withDefault [ 456 ])

        ( _, puzzle ) =
            makePuzzle seed0 quotation.text
    in
    ( { puzzle = puzzle
      , quotation = quotation
      , history = []
      , hovered = Nothing
      , selected = Nothing
      , hasMouse = flags.hasMouse
      }
    , Cmd.none
    )


vowelList : List Char
vowelList =
    "aeiou" |> String.toList


isVowel : Char -> Bool
isVowel c =
    List.member c vowelList


isnt : comparable -> comparable -> Bool
isnt x y =
    x /= y


makePuzzle : Pcg.Seed -> String -> ( Pcg.Seed, String )
makePuzzle seed0 solution =
    let
        uniqueChars =
            solution |> String.toLower |> String.toList |> List.filter Char.isAlpha |> Set.fromList |> Set.toList

        categorize : Char -> { vowels : List Char, consonants : List Char } -> { vowels : List Char, consonants : List Char }
        categorize c cats =
            if isVowel c then
                { cats | vowels = c :: cats.vowels }

            else
                { cats | consonants = c :: cats.consonants }

        { vowels, consonants } =
            uniqueChars |> List.foldl categorize { vowels = [], consonants = [] }

        ( lowerVowelScrambler, seed1 ) =
            makeScrambler vowels seed0

        ( lowerConsonantScrambler, seed2 ) =
            makeScrambler consonants seed1

        withUppers =
            List.concatMap (\pair -> [ pair, pair |> mapBoth Char.toUpper Char.toUpper ])

        scrambler =
            (lowerVowelScrambler ++ lowerConsonantScrambler)
                |> withUppers
                |> Dict.fromList

        scramble c =
            Dict.get c scrambler |> withDefault c
    in
    ( seed2, String.map scramble solution )


type alias ScramblerBuild comparable =
    { seed : Pcg.Seed
    , xCount : Int
    , xs : List comparable
    , scrambler : List ( comparable, comparable )
    }


{-| Create a mapping of each element of `inputs` to a random element of `inputs` (without duplicates).
-}
makeScrambler : List comparable -> Pcg.Seed -> ( List ( comparable, comparable ), Pcg.Seed )
makeScrambler inputs seed0 =
    let
        step : comparable -> ScramblerBuild comparable -> ScramblerBuild comparable
        step k state =
            case state.xs of
                [] ->
                    state

                [ v ] ->
                    { seed = state.seed
                    , xCount = 0
                    , xs = []
                    , scrambler = ( k, v ) :: state.scrambler
                    }

                _ ->
                    -- Try not to map k to itself. This guarantees that the scrambling won't map every element to itself, unless there are only 0 or 1 elements.
                    let
                        kList =
                            if List.member k state.xs then
                                [ k ]

                            else
                                []

                        candidates =
                            List.filter (isnt k) state.xs

                        gen =
                            Pcg.int 0 (List.length candidates - 1)

                        ( i, nextSeed ) =
                            Pcg.step gen state.seed

                        tail =
                            List.drop i candidates
                    in
                    case tail of
                        [] ->
                            state

                        v :: rest ->
                            { seed = nextSeed
                            , xCount = state.xCount - 1
                            , xs = List.take i candidates ++ rest ++ kList
                            , scrambler = ( k, v ) :: state.scrambler
                            }

        initialState =
            { seed = seed0, xCount = List.length inputs, xs = inputs, scrambler = [] }

        finalState =
            List.foldl step initialState inputs
    in
    ( finalState.scrambler, finalState.seed )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( applyMsg msg model |> checkSolved, Cmd.none )


applyMsg : Msg -> Model -> Model
applyMsg msg model =
    let
        hoverFor c =
            case model.selected of
                Nothing ->
                    Just c

                Just d ->
                    if canSwap c d then
                        Just c

                    else
                        Nothing
    in
    case msg of
        Click c ->
            applyClick model c

        Ignore ->
            model

        MouseEnter c ->
            { model | hovered = hoverFor c }

        MouseLeave c ->
            { model | hovered = model.hovered |> Maybe2.filter (isnt c) }

        Undo ->
            applyUndo model


canSwap : Char -> Char -> Bool
canSwap c d =
    isVowel c == isVowel d


isSolved : Model -> Bool
isSolved model =
    model.puzzle == model.quotation.text


applyClick : Model -> Char -> Model
applyClick model c =
    case model.selected of
        Nothing ->
            { model | selected = Just c }

        Just s ->
            if canSwap s c then
                let
                    newPuzzle =
                        String.map (charSwap c s) model.puzzle

                    newHovered =
                        model.hovered |> Maybe.map (\_ -> s)
                in
                { model
                    | selected = Nothing
                    , hovered = newHovered
                }
                    |> setPuzzle newPuzzle

            else
                model


setPuzzle : String -> Model -> Model
setPuzzle puzzle model =
    if puzzle == model.puzzle then
        model

    else
        { model | puzzle = puzzle, history = model.puzzle :: model.history }


checkSolved : Model -> Model
checkSolved model =
    if isSolved model then
        { model | hovered = Nothing, selected = Nothing }

    else
        model


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
        hideIfSolved =
            Element.transparent <| isSolved model

        paragraph s =
            Element.paragraph [ hideIfSolved ] [ text s ]

        verb =
            clickVerb model
    in
    [ "Can you unscramble the letters of the following quotation?" |> paragraph
    , titlize verb
        ++ " two letters to swap them. For example, "
        ++ verb
        ++ " ‘x’ and ‘y’ to replace every ‘x’ with ‘y’ and vice-versa. If you "
        ++ verb
        ++ " a letter to start a swap, but then change your mind, "
        ++ verb
        ++ " the letter again to deselect it. You can only swap vowels with vowels, and consonants with consonants."
        |> paragraph
    , puzzleView model
    , "—" ++ model.quotation.author |> text |> el [ Element.alignRight ]
    , "If you're having trouble finding a letter in the puzzle above, check the list of letters below. "
        ++ "The list only contains letters used in the puzzle. You can also "
        ++ clickVerb model
        ++ " the letters in the list!"
        |> paragraph
    , inventoryView model |> el [ centerX, hideIfSolved ]
    , "The number under each letter is how many times that letter currently appears in the puzzle. "
        ++ "The numbers will change as you swap letters."
        |> paragraph
    , undoButton model |> el [ hideIfSolved ]
    ]
        |> column
            [ centerX
            , Element.centerY
            , Element.spacing 30
            ]
        |> Element.layout
            [ style "padding" "3em"
            ]



{--
hint : Model -> String
hint model =
    let
        pairs =
            List.map2 (\a b -> ( a, b )) (model.puzzle |> String.toLower |> String.toList) (model.quotation.text |> String.toLower |> String.toList)

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
--}


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
            [ centerX
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
            (charStyle ++ [ centerX, unselectable ])


wordView : { a | hasMouse : Bool, hovered : Maybe Char, selected : Maybe Char } -> String -> Element Msg
wordView model word =
    word
        |> String.toList
        |> List.map (charView model)
        |> row [ unselectable ]


inventoryView : Model -> Element Msg
inventoryView model =
    let
        incrementCount maybeCount =
            case maybeCount of
                Nothing ->
                    Just 1

                Just count ->
                    Just (count + 1)

        updateCounts c counts =
            Dict.update c incrementCount counts

        countedChars =
            model.puzzle
                |> String.toLower
                |> String.filter Char.isAlpha
                |> String.toList
                |> List.foldl updateCounts Dict.empty
                |> Dict.toList
                |> List.sort

        ( countedVowels, countedConsonants ) =
            List.partition (Tuple.first >> isVowel) countedChars

        vowelRow =
            List.map (countedCharView model) countedVowels |> row []

        consonantRow =
            List.map (countedCharView model) countedConsonants |> wrappedRow [ Element.width Element.fill ]
    in
    row
        [ unselectable
        , Element.spacing 30
        ]
        [ vowelRow
        , consonantRow
        ]


countedCharView : { a | hasMouse : Bool, hovered : Maybe Char, selected : Maybe Char } -> ( Char, Int ) -> Element Msg
countedCharView model ( c, count ) =
    column
        []
        [ charView model c
            |> el
                (charStyle ++ [ centerX ])
        , String.fromInt count
            |> text
            |> el
                [ Font.size 16
                , centerX
                ]
        ]


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


charStyle : List (Element.Attribute msg)
charStyle =
    [ Font.family [ Font.monospace ]
    , Font.size 34
    ]


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute <| Html.Attributes.style name value
