port module App exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Font as Font
import Element.Lazy
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List2
import Maybe.Extra as Maybe2
import Playing exposing (PointerType)
import Puzzle exposing (Puzzle)
import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>))
import Url.Parser.Query as QP


type alias Model =
    { navigationKey : Nav.Key
    , pointer : PointerType
    , puzzles : List Puzzle
    , route : Route
    , url : Url
    }


type Msg
    = DateWasClicked String
    | LinkWasClicked UrlRequest
    | PlayingMsg Playing.Msg
    | DidReceiveJavascriptResponse E.Value
    | UrlDidChange Url


type Route
    = LoadingRoute Puzzle
    | NoPuzzlesRoute
    | PlayingRoute Playing.Model


type alias AppFlags =
    { hasMouse : Bool
    , puzzles : List Puzzle
    }


type JavascriptRequest
    = GetText { date : String }
    | SetText { date : String, text : String }


encodeJavascriptRequest : JavascriptRequest -> E.Value
encodeJavascriptRequest request =
    case request of
        GetText { date } ->
            E.object
                [ ( "date", E.string date )
                , ( "type", E.string "GetText" )
                ]

        SetText { date, text } ->
            E.object
                [ ( "date", E.string date )
                , ( "text", E.string text )
                , ( "type", E.string "SetText" )
                ]


port javascriptRequest : E.Value -> Cmd msg


type alias DidGetTextResponse =
    { date : String
    , text : String
    }


type JavascriptResponse
    = DidGetText DidGetTextResponse


decodeJavascriptResponse : D.Value -> Maybe JavascriptResponse
decodeJavascriptResponse =
    let
        didGetTextDecoder =
            D.map2 DidGetTextResponse
                (D.field "date" D.string)
                (D.field "text" D.string)

        decoder =
            D.field "type" D.string
                |> D.andThen
                    (\responseType ->
                        case responseType of
                            "DidGetText" ->
                                D.map DidGetText didGetTextDecoder

                            _ ->
                                D.fail ("unknown type" ++ responseType)
                    )
    in
    D.decodeValue decoder >> Result.toMaybe


port javascriptResponse : (D.Value -> msg) -> Sub msg


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
        pointer =
            if flags.hasMouse then
                Playing.Mouse

            else
                Playing.Touch

        model0 : Model
        model0 =
            { navigationKey = key
            , pointer = pointer
            , puzzles = flags.puzzles
            , route = NoPuzzlesRoute
            , url = url
            }

        route =
            dateForUrl url |> routeForDate model0

        model1 =
            { model0 | route = route }
    in
    ( model1
    , updateUrlCmd model1
    )


routeForDate : Model -> Maybe String -> Route
routeForDate model maybeDate =
    case puzzleForDate model.puzzles maybeDate of
        Nothing ->
            NoPuzzlesRoute

        Just puzzle ->
            PlayingRoute (Playing.init puzzle puzzle.text model.pointer)


dateForUrl : Url -> Maybe String
dateForUrl =
    let
        parser =
            UP.s "~mayoff"
                </> UP.s "DeliaGame"
                <?> QP.string "date"
    in
    UP.parse parser >> Maybe2.join


puzzleForUrl : List Puzzle -> Url -> Maybe Puzzle
puzzleForUrl puzzles url =
    dateForUrl url |> puzzleForDate puzzles


puzzleForDate : List Puzzle -> Maybe String -> Maybe Puzzle
puzzleForDate puzzles maybeDate =
    maybeDate
        |> Maybe.andThen (\date -> List2.find (\puzzle -> puzzle.date == date) puzzles)
        |> Maybe2.orElse (List2.last puzzles)


updateUrlCmd : Model -> Cmd Msg
updateUrlCmd model =
    case model.route of
        NoPuzzlesRoute ->
            Cmd.none

        PlayingRoute playing ->
            updateUrlWithDateCmd model (Playing.puzzleDate playing)

        LoadingRoute puzzle ->
            updateUrlWithDateCmd model puzzle.date


urlSetDate : Url -> String -> Url
urlSetDate url date =
    { url | query = Just ("date=" ++ date) }


updateUrlWithDateCmd : Model -> String -> Cmd Msg
updateUrlWithDateCmd model date =
    let
        oldUrl =
            model.url

        newUrl =
            urlSetDate oldUrl date
    in
    if newUrl.query == model.url.query then
        Cmd.none

    else if model.url.query == Nothing then
        Nav.replaceUrl model.navigationKey (Url.toString newUrl)

    else
        Nav.pushUrl model.navigationKey (Url.toString newUrl)


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
        DateWasClicked date ->
            dateWasClicked date model

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
        oldText =
            Playing.puzzleText playing

        newPlaying =
            Playing.update msg playing

        saveCmd =
            if oldText == Playing.puzzleText newPlaying then
                Cmd.none

            else
                SetText
                    { date = Playing.puzzleDate newPlaying
                    , text = Playing.puzzleText newPlaying
                    }
                    |> encodeJavascriptRequest
                    |> javascriptRequest
    in
    ( { model | route = PlayingRoute newPlaying }
    , saveCmd
    )


dateWasClicked : String -> Model -> ( Model, Cmd Msg )
dateWasClicked date model =
    pure { model | route = routeForDate model (Just date) }


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
        ( Just (DidGetText { date, text }), LoadingRoute puzzle ) ->
            if puzzle.date /= date then
                ( model, Cmd.none )

            else
                ( { model | route = PlayingRoute (Playing.init puzzle text model.pointer) }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


urlDidChange : Url -> Model -> ( Model, Cmd Msg )
urlDidChange url model =
    pure { model | route = routeForDate model (dateForUrl url), url = url }



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
                    ( Element.text (puzzle.date ++ " (Loading)")
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
    Element.column []
        [ dayPicker
        , element
        ]
        |> Element.layout []


dayPickerView url currentDate puzzles =
    let
        pickPrior : String -> Maybe String -> Maybe String
        pickPrior date prior =
            case ( date < currentDate, prior ) of
                ( False, _ ) ->
                    prior

                ( True, Nothing ) ->
                    Just date

                ( True, Just priorDate ) ->
                    Just (max date priorDate)

        pickNext : String -> Maybe String -> Maybe String
        pickNext date next =
            case ( date > currentDate, next ) of
                ( False, _ ) ->
                    next

                ( True, Nothing ) ->
                    Just date

                ( True, Just nextDate ) ->
                    Just (min date nextDate)

        pickDates : Puzzle -> { prior : Maybe String, next : Maybe String } -> { prior : Maybe String, next : Maybe String }
        pickDates { date } { prior, next } =
            { prior = pickPrior date prior, next = pickNext date next }

        dates =
            List.foldl pickDates { prior = Nothing, next = Nothing } puzzles
    in
    Element.row
        [ Element.spacing 30
        , Element.centerX
        , Element.padding 30
        ]
        [ dates.prior |> Maybe.map (urlSetDate url) |> dayLink "Previous"
        , Element.text currentDate
        , dates.next |> Maybe.map (urlSetDate url) |> dayLink "Next"
        ]


dayLink : String -> Maybe Url -> Element Msg
dayLink label maybeUrl =
    Element.link
        [ Element.transparent (Maybe2.isNothing maybeUrl)
        ]
        { url = maybeUrl |> Maybe.map Url.toString |> Maybe.withDefault ""
        , label =
            Element.text label
                |> Element.el
                    [ Font.color <| Element.rgb 0.2 0.2 1.0
                    , Font.underline
                    ]
        }
