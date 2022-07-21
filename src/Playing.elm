module Playing exposing
    ( Model
    , Msg
    , PersistentState
    , PointerType(..)
    , encodePersistentState
    , init
    , persistentStateDecoder
    , persistentStateForModel
    , puzzleDate
    , update
    , view
    )

import Dict
import Element exposing (Element, centerX, column, el, padding, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Maybe.Extra as Maybe2
import Puzzle exposing (Date, Puzzle)
import SHA256
import Set exposing (Set)


type Model
    = Model Privates


type PointerType
    = Mouse
    | Touch


type alias Privates =
    { puzzle : Puzzle
    , originalText : String
    , locks : Set Char
    , history : List UndoEntry
    , hovered : Maybe Char
    , selected : Maybe Char
    , pointer : PointerType
    }


type alias UndoEntry =
    { text : String
    , locks : Set Char
    }


init : Puzzle -> Maybe PersistentState -> PointerType -> Model
init puzzle maybeState pointer =
    let
        ( locks, text ) =
            case maybeState of
                Nothing ->
                    ( Set.empty, puzzle.text )

                Just (PersistentState state) ->
                    ( state.locks, state.text )
    in
    Model
        { puzzle = { puzzle | text = text }
        , originalText = puzzle.text
        , locks = locks
        , history = []
        , hovered = Nothing
        , selected = Nothing
        , pointer = pointer
        }


puzzleDate : Model -> Date
puzzleDate (Model model) =
    model.puzzle.date


type Msg
    = Click Char
    | Ignore
    | MouseEnter Char
    | MouseLeave Char
    | Reset
    | SetLocked Char Bool
    | Undo


update : Msg -> Model -> Model
update msg (Model model) =
    applyMsg msg model |> checkSolved model |> Model


applyMsg : Msg -> Privates -> Privates
applyMsg msg model =
    let
        hoverFor c =
            if Set.member c model.locks then
                Nothing

            else
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

        Reset ->
            let
                puzzle =
                    model.puzzle
            in
            { model
                | puzzle = { puzzle | text = model.originalText }
                , locks = Set.empty
                , hovered = Nothing
                , selected = Nothing
            }

        SetLocked c isLocked ->
            setLocked model c isLocked

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


undoStateForModel : Privates -> UndoEntry
undoStateForModel model =
    { text = model.puzzle.text
    , locks = model.locks
    }


checkSolved : Privates -> Privates -> Privates
checkSolved oldModel model =
    if oldModel.puzzle.text /= model.puzzle.text && isSolved model then
        { model
            | hovered = Nothing
            , selected = Nothing
            , locks = Set.empty
        }

    else
        model


isSolved : Privates -> Bool
isSolved model =
    let
        textHash =
            model.puzzle.text
                |> SHA256.fromString
                |> SHA256.toHex
    in
    textHash == model.puzzle.hash


applyClick : Privates -> Char -> Privates
applyClick model c =
    if Set.member c model.locks then
        model

    else
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
            , history = undoStateForModel model :: model.history
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


setLocked : Privates -> Char -> Bool -> Privates
setLocked model c isLocked =
    if Set.member c model.locks == isLocked then
        model

    else
        { model
            | locks =
                if isLocked then
                    Set.insert c model.locks

                else
                    Set.remove c model.locks
            , history = undoStateForModel model :: model.history
            , selected = model.selected |> Maybe2.filter (isnt c)
            , hovered = model.hovered |> Maybe2.filter (isnt c)
        }


applyUndo : Privates -> Privates
applyUndo model =
    case model.history of
        first :: rest ->
            let
                puzzle =
                    model.puzzle
            in
            { model
                | puzzle = { puzzle | text = first.text }
                , locks = first.locks
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
    , makeTitleCase verb
        ++ " a lock (🔒) to ‘lock in’ a letter you're sure about, "
        ++ "so you don't accidentally swap it. You can "
        ++ verb
        ++ " the lock again to unlock the letter if you change your mind."
        |> paragraph
    , row [ hideIfSolved, centerX ]
        [ undoButton model
        , resetButton
        ]
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


resetButton : Element Msg
resetButton =
    Html.button
        [ Html.Events.onClick Reset
        , Html.Attributes.style "font-size" "24px"
        ]
        [ Html.text "Reset"
        ]
        |> Element.html
        |> el
            [ unselectable ]


puzzleView : Privates -> Element Msg
puzzleView model =
    model.puzzle.text
        |> String.lines
        |> List.map (lineView model)
        |> column [ centerX ]


lineView : Privates -> String -> Element Msg
lineView model line =
    let
        leadingSpaces =
            line |> String.toList |> List.Extra.takeWhile isSpace |> String.fromList

        words =
            line
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
    ([ leadingSpaces ] ++ words)
        |> List.map (wordView model)
        |> wrappedRow
            (charStyle ++ [ Element.alignLeft, unselectable ])


isSpace : Char -> Bool
isSpace c =
    c == ' '


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
    let
        isLocked =
            Set.member c model.locks

        lockButtonAttributes =
            if isLocked then
                []

            else
                [ style "filter" "grayscale(100%)"
                , style "-webkit-filter" "grayscale(100%)"
                , Element.alpha 0.5
                ]
    in
    column
        []
        [ Element.Input.button ([ centerX ] ++ lockButtonAttributes)
            { onPress = Just (SetLocked c (not isLocked))
            , label = text "🔒"
            }
        , charView model c
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
        isLocked =
            Set.member (Char.toLower c) model.locks

        bold =
            if isLocked then
                [ Font.bold ]

            else
                []

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
                ++ (extras ++ bold)
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



-- PERSISTENT STATE


type PersistentState
    = PersistentState PersistentStateRecord


type alias PersistentStateRecord =
    { locks : Set Char
    , text : String
    }


persistentStateForModel : Model -> PersistentState
persistentStateForModel (Model model) =
    PersistentState
        { locks = model.locks
        , text = model.puzzle.text
        }


encodePersistentState : PersistentState -> String
encodePersistentState (PersistentState { locks, text }) =
    E.encode 0
        (E.object
            [ ( "locks", locks |> Set.toList |> String.fromList |> E.string )
            , ( "text", E.string text )
            ]
        )


persistentStateDecoder : D.Decoder PersistentState
persistentStateDecoder =
    let
        locksDecoder : D.Decoder (Set Char)
        locksDecoder =
            D.string |> D.map (String.toList >> Set.fromList)
    in
    D.map2 PersistentStateRecord
        (D.field "locks" locksDecoder)
        (D.field "text" D.string)
        |> D.map PersistentState
