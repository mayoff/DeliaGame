port module App exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Font as Font
import Element.Lazy
import Html exposing (Html)
import Html.Attributes
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe2
import Playing exposing (PointerType, persistentStateForModel)
import Puzzle exposing (Date, Puzzle, dateFromIsoDateTime)
import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>))
import Url.Parser.Query as QP


type alias Model =
    { navigationKey : Nav.Key
    , pointer : PointerType
    , puzzles : Array Puzzle
    , route : Route
    , url : Url
    }


type Msg
    = LinkWasClicked UrlRequest
    | PlayingMsg Playing.Msg
    | DidReceiveJavascriptResponse E.Value
    | UrlDidChange Url


type Route
    = LoadingRoute Puzzle
    | NoPuzzlesRoute
    | PlayingRoute Playing.Model


type alias AppFlags =
    { isoDateTime : String
    , hasMouse : Bool
    , puzzles : List Puzzle
    }


main : Program AppFlags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlDidChange
        , onUrlRequest = LinkWasClicked
        , subscriptions = subscriptions
        , update = update

        --, update = \msg model -> update (Debug.log "msg" msg) model |> Debug.log "result"
        , view = document
        }


init : AppFlags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        date =
            dateFromIsoDateTime flags.isoDateTime

        puzzles =
            flags.puzzles
                |> List.filter (\puzzle -> puzzle.date <= date)
                |> List.sortBy .date
                |> Array.fromList

        pointer =
            if flags.hasMouse then
                Playing.Mouse

            else
                Playing.Touch

        model0 : Model
        model0 =
            { navigationKey = key
            , pointer = pointer
            , puzzles = puzzles
            , route = NoPuzzlesRoute
            , url = url
            }

        ( route, cmd ) =
            dateForUrl url |> routeForDate model0

        model1 =
            { model0 | route = route }
    in
    ( model1
    , Cmd.batch [ updateUrlCmd model1, cmd ]
    )


routeForDate : Model -> Maybe Date -> ( Route, Cmd Msg )
routeForDate model maybeDate =
    case puzzleForDate model.puzzles maybeDate of
        Nothing ->
            pure NoPuzzlesRoute

        Just puzzle ->
            ( LoadingRoute puzzle
            , getStateCmd puzzle.date
            )


dateForUrl : Url -> Maybe Date
dateForUrl =
    let
        parser =
            UP.top
                <?> QP.string "date"
    in
    \url ->
        -- I don't actually care about the path, just the query params.
        { url | path = "/" } |> UP.parse parser |> Maybe2.join


puzzleForDate : Array Puzzle -> Maybe Date -> Maybe Puzzle
puzzleForDate puzzles maybeDate =
    maybeDate
        |> Maybe.andThen
            (\date ->
                puzzles
                    |> Array.filter (\puzzle -> puzzle.date == date)
                    |> Array.get 0
            )
        |> Maybe2.orElse (arrayLast puzzles)


updateUrlCmd : Model -> Cmd Msg
updateUrlCmd model =
    case model.route of
        NoPuzzlesRoute ->
            Cmd.none

        PlayingRoute playing ->
            updateUrlWithDateCmd model (Playing.puzzleDate playing)

        LoadingRoute puzzle ->
            updateUrlWithDateCmd model puzzle.date


updateUrlWithDateCmd : Model -> Date -> Cmd Msg
updateUrlWithDateCmd model date =
    let
        url =
            model.url

        dateParam =
            "date=" ++ date

        isDateOfLatestPuzzle =
            Just date == (arrayLast model.puzzles |> Maybe.map .date)

        isAcceptableQuery =
            (url.query == Nothing && isDateOfLatestPuzzle)
                || (url.query == Just dateParam)
    in
    if isAcceptableQuery then
        Cmd.none

    else
        let
            newQuery =
                if isDateOfLatestPuzzle then
                    Nothing

                else
                    Just dateParam
        in
        Nav.pushUrl model.navigationKey ({ url | query = newQuery } |> Url.toString)


urlSetDate : Url -> Date -> Url
urlSetDate url date =
    { url | query = Just ("date=" ++ date) }


urlRemoveDate : Url -> Url
urlRemoveDate url =
    { url | query = Nothing }


subscriptions : Model -> Sub.Sub Msg
subscriptions _ =
    javascriptResponse DidReceiveJavascriptResponse



-- UPDATE


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            applyMsg msg model
    in
    ( newModel, Cmd.batch [ cmd, updateUrlCmd newModel ] )


applyMsg : Msg -> Model -> ( Model, Cmd Msg )
applyMsg msg model =
    case msg of
        LinkWasClicked request ->
            linkWasClicked request model

        PlayingMsg playingMsg ->
            case model.route of
                PlayingRoute playingModel ->
                    applyPlayingMsg playingMsg model playingModel

                _ ->
                    pure model

        DidReceiveJavascriptResponse value ->
            didReceiveJavascriptResponse value model

        UrlDidChange url ->
            urlDidChange url model


applyPlayingMsg : Playing.Msg -> Model -> Playing.Model -> ( Model, Cmd Msg )
applyPlayingMsg msg model playing =
    let
        newPlaying =
            Playing.update msg playing

        newState =
            persistentStateForModel newPlaying

        saveCmd =
            if persistentStateForModel playing == newState then
                Cmd.none

            else
                setStateCmd (Playing.puzzleDate playing) newState
    in
    ( { model | route = PlayingRoute newPlaying }
    , saveCmd
    )


linkWasClicked : UrlRequest -> Model -> ( Model, Cmd Msg )
linkWasClicked request model =
    case request of
        Browser.Internal url ->
            ( model, Nav.pushUrl model.navigationKey (Url.toString url) )

        Browser.External string ->
            ( model, Nav.load string )


didReceiveJavascriptResponse : D.Value -> Model -> ( Model, Cmd Msg )
didReceiveJavascriptResponse value model =
    case ( decodeJavascriptResponse value, model.route ) of
        ( Just (LocalStorageDidGet key maybeValue), LoadingRoute puzzle ) ->
            if localStorageKeyForDate puzzle.date /= key then
                pure model

            else
                let
                    maybeState =
                        maybeValue
                            |> Maybe.map (D.decodeString Playing.persistentStateDecoder)
                            |> Maybe.map Result.toMaybe
                            |> Maybe2.join
                in
                pure { model | route = PlayingRoute (Playing.init puzzle maybeState model.pointer) }

        _ ->
            pure model


urlDidChange : Url -> Model -> ( Model, Cmd Msg )
urlDidChange url model =
    let
        ( route, cmd ) =
            routeForDate model (dateForUrl url)
    in
    ( { model | route = route, url = url }, cmd )



-- VIEW


document : Model -> Document Msg
document model =
    { title = title model
    , body = [ view model ]
    }


title : Model -> String
title model =
    let
        prefix =
            "Delia's Game"
    in
    case model.route of
        LoadingRoute puzzle ->
            prefix ++ " for " ++ puzzle.date ++ " (Loading)"

        NoPuzzlesRoute ->
            prefix ++ " (No Puzzles)"

        PlayingRoute playing ->
            prefix ++ " for " ++ Playing.puzzleDate playing


view : Model -> Html Msg
view model =
    let
        dayPickerForDate date =
            Element.Lazy.lazy3 dayPickerView model.url date model.puzzles

        ( element, dayPicker ) =
            case model.route of
                LoadingRoute puzzle ->
                    ( Element.text "(Loading)" |> Element.el [ Element.centerX ]
                    , dayPickerForDate puzzle.date
                    )

                NoPuzzlesRoute ->
                    ( Element.text "No Puzzles"
                    , Element.none
                    )

                PlayingRoute playing ->
                    ( Playing.view playing
                        |> Element.map PlayingMsg
                    , dayPickerForDate (Playing.puzzleDate playing)
                    )
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 30
        ]
        [ dayPicker
        , element
        , "https://github.com/mayoff/DeliaGame" |> Url.fromString |> link "github"
        ]
        |> Element.layout
            [ style "padding" "3em"
            ]


dayPickerView : Url -> Date -> Array Puzzle -> Element Msg
dayPickerView url currentDate puzzles =
    let
        pickPrior : Date -> Maybe Date -> Maybe Date
        pickPrior date prior =
            case ( date < currentDate, prior ) of
                ( False, _ ) ->
                    prior

                ( True, Nothing ) ->
                    Just date

                ( True, Just priorDate ) ->
                    Just (max date priorDate)

        pickNext : Date -> Maybe Date -> Maybe Date
        pickNext date next =
            case ( date > currentDate, next ) of
                ( False, _ ) ->
                    next

                ( True, Nothing ) ->
                    Just date

                ( True, Just nextDate ) ->
                    Just (min date nextDate)

        pickDates : Puzzle -> { prior : Maybe Date, next : Maybe Date } -> { prior : Maybe Date, next : Maybe Date }
        pickDates { date } { prior, next } =
            { prior = pickPrior date prior, next = pickNext date next }

        dates : { prior : Maybe Date, next : Maybe Date }
        dates =
            Array.foldl pickDates { prior = Nothing, next = Nothing } puzzles

        middle =
            [ dates.prior |> Maybe.map (urlSetDate url) |> link "Previous"
            , Element.text currentDate
            , dates.next |> Maybe.map (urlSetDate url) |> link "Next"
            ]
                |> Element.row
                    [ Element.spacing 30
                    , Element.centerX
                    ]
    in
    [ puzzles |> Array.get 0 |> Maybe.map .date |> Maybe.map (urlSetDate url) |> link "First"
    , middle
    , Just (urlRemoveDate url) |> link "Latest"
    ]
        |> Element.row [ Element.width Element.fill ]


link : String -> Maybe Url -> Element Msg
link label maybeUrl =
    let
        labelElement =
            Element.text label
                |> Element.el
                    [ Font.color <| Element.rgb 0.2 0.2 1.0
                    , Font.underline
                    ]
    in
    case maybeUrl of
        Nothing ->
            labelElement |> Element.el [ Element.transparent True ]

        Just url ->
            Element.link []
                { url = Url.toString url
                , label = labelElement
                }


style : String -> String -> Element.Attribute msg
style name value =
    Element.htmlAttribute <| Html.Attributes.style name value


arrayLast : Array a -> Maybe a
arrayLast array =
    Array.get (Array.length array - 1) array



-- PORTS


type LocalStorageKey
    = LocalStorageKey String


encodeLocalStorageKey : LocalStorageKey -> E.Value
encodeLocalStorageKey (LocalStorageKey key) =
    E.string key


localStorageKeyDecoder : D.Decoder LocalStorageKey
localStorageKeyDecoder =
    D.string |> D.map LocalStorageKey


type JavascriptRequest
    = LocalStorageGet LocalStorageKey
    | LocalStorageSet LocalStorageKey String


encodeJavascriptRequest : JavascriptRequest -> E.Value
encodeJavascriptRequest request =
    case request of
        LocalStorageGet key ->
            E.object
                [ ( "key", encodeLocalStorageKey key )
                , ( "type", E.string "LocalStorageGet" )
                ]

        LocalStorageSet key value ->
            E.object
                [ ( "key", encodeLocalStorageKey key )
                , ( "type", E.string "LocalStorageSet" )
                , ( "value", E.string value )
                ]


port javascriptRequest : E.Value -> Cmd msg


type JavascriptResponse
    = LocalStorageDidGet LocalStorageKey (Maybe String)


decodeJavascriptResponse : D.Value -> Maybe JavascriptResponse
decodeJavascriptResponse =
    let
        localStorageDidGetDecoder =
            D.map2 LocalStorageDidGet
                (D.field "key" localStorageKeyDecoder)
                (D.field "value" (D.nullable D.string))

        decoder =
            D.field "type" D.string
                |> D.andThen
                    (\responseType ->
                        case responseType of
                            "LocalStorageDidGet" ->
                                localStorageDidGetDecoder

                            _ ->
                                D.fail ("unknown type" ++ responseType)
                    )
    in
    D.decodeValue decoder >> Result.toMaybe


port javascriptResponse : (D.Value -> msg) -> Sub msg



-- PERSISTENT STATE


localStorageKeyForDate : Date -> LocalStorageKey
localStorageKeyForDate date =
    LocalStorageKey ("scrambled-state-" ++ date)


getStateCmd : Date -> Cmd Msg
getStateCmd date =
    LocalStorageGet (localStorageKeyForDate date) |> encodeJavascriptRequest |> javascriptRequest


setStateCmd : Date -> Playing.PersistentState -> Cmd Msg
setStateCmd date state =
    LocalStorageSet (localStorageKeyForDate date) (state |> Playing.encodePersistentState) |> encodeJavascriptRequest |> javascriptRequest
