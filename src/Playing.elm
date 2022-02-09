module Playing exposing
    ( Model
    , Msg
    , PointerType(..)
    , init
    , puzzleDate
    , puzzleText
    , update
    , view
    )

import Dict
import Element exposing (Element, centerX, column, el, padding, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html
import Html.Attributes
import Html.Events
import Maybe.Extra as Maybe2
import Puzzle exposing (Puzzle)
import Sha256


type Model
    = Model Privates


type PointerType
    = Mouse
    | Touch


type alias Privates =
    { puzzle : Puzzle
    , history : List String
    , hovered : Maybe Char
    , selected : Maybe Char
    , pointer : PointerType
    }


init : Puzzle -> String -> PointerType -> Model
init puzzle text pointer =
    Model
        { puzzle = { puzzle | text = text }
        , history = []
        , hovered = Nothing
        , selected = Nothing
        , pointer = pointer
        }


puzzleText : Model -> String
puzzleText (Model model) =
    model.puzzle.text


puzzleDate : Model -> String
puzzleDate (Model model) =
    model.puzzle.date


type Msg
    = Click Char
    | Ignore
    | MouseEnter Char
    | MouseLeave Char
    | Undo


update : Msg -> Model -> Model
update msg (Model model) =
    applyMsg msg model |> checkSolved |> Model


applyMsg : Msg -> Privates -> Privates
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


isnt : comparable -> comparable -> Bool
isnt x y =
    x /= y


isVowel : Char -> Bool
isVowel =
    let
        vowels =
            "aeiou" |> String.toList
    in
    \c -> List.member c vowels


checkSolved : Privates -> Privates
checkSolved model =
    if isSolved model then
        { model | hovered = Nothing, selected = Nothing }

    else
        model


isSolved : Privates -> Bool
isSolved model =
    Sha256.sha256 model.puzzle.text == model.puzzle.hash


applyClick : Privates -> Char -> Privates
applyClick model c =
    case model.selected of
        Nothing ->
            { model | selected = Just c }

        Just s ->
            if canSwap s c then
                let
                    newText =
                        String.map (charSwap c s) model.puzzle.text

                    newHovered =
                        model.hovered |> Maybe.map (\_ -> s)
                in
                { model
                    | selected = Nothing
                    , hovered = newHovered
                }
                    |> setPuzzleText newText

            else
                model


setPuzzleText : String -> Privates -> Privates
setPuzzleText text model =
    let
        puzzle =
            model.puzzle
    in
    if text == puzzle.text then
        model

    else
        { model
            | puzzle = { puzzle | text = text }
            , history = puzzle.text :: model.history
        }


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


applyUndo : Privates -> Privates
applyUndo model =
    case model.history of
        first :: rest ->
            let
                puzzle =
                    model.puzzle
            in
            { model
                | puzzle = { puzzle | text = first }
                , history = rest
            }

        [] ->
            model


view : Model -> Element Msg
view (Model model) =
    let
        hideIfSolved =
            Element.transparent <| isSolved model

        paragraph s =
            Element.paragraph [ hideIfSolved ] [ text s ]

        verb =
            clickVerb model.pointer
    in
    [ "Can you unscramble the letters of the following quotation?" |> paragraph
    , makeTitleCase verb
        ++ " two letters to swap them. For example, "
        ++ verb
        ++ " ‘x’ and ‘y’ to replace every ‘x’ with ‘y’ and vice-versa. If you "
        ++ verb
        ++ " a letter to start a swap, but then change your mind, "
        ++ verb
        ++ " the letter again to deselect it. You can only swap vowels with vowels, and consonants with consonants."
        |> paragraph
    , puzzleView model
    , "—" ++ model.puzzle.author |> text |> el [ Element.alignRight ]
    , "If you're having trouble finding a letter in the puzzle above, check the list of letters below. "
        ++ "The list only contains letters used in the puzzle. You can also "
        ++ verb
        ++ " the letters in the list!"
        |> paragraph
    , inventoryView model |> el [ centerX, hideIfSolved ]
    , "The number under each letter is how many times that letter currently appears in the puzzle. "
        ++ "The numbers will change as you swap letters."
        |> paragraph
    , undoButton model |> el [ hideIfSolved, centerX ]
    ]
        |> column
            [ centerX
            , Element.centerY
            , Element.spacing 30
            ]


clickVerb : PointerType -> String
clickVerb pointer =
    case pointer of
        Mouse ->
            "click"

        Touch ->
            "tap"


makeTitleCase : String -> String
makeTitleCase s =
    String.append (s |> String.left 1 |> String.toUpper) (s |> String.dropLeft 1)


undoButton : Privates -> Element Msg
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
            [ unselectable ]


puzzleView : Privates -> Element Msg
puzzleView model =
    let
        words =
            model.puzzle.text
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


wordView : Privates -> String -> Element Msg
wordView model word =
    word
        |> String.toList
        |> List.map (charView model)
        |> row [ unselectable ]


inventoryView : Privates -> Element Msg
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
            model.puzzle.text
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


countedCharView : Privates -> ( Char, Int ) -> Element Msg
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


charView : Privates -> Char -> Element Msg
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
            case model.pointer of
                Touch ->
                    []

                Mouse ->
                    [ Events.onMouseEnter (MouseEnter cLower)
                    , Events.onMouseLeave (MouseLeave cLower)
                    , cursor cursorName
                    ]

        extras =
            if Char.isAlpha c then
                hoverEvents
                    ++ [ Events.onClick (Click cLower)
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
