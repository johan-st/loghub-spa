module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field)
import String exposing (fromInt)
import Url



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : State
    }


type alias FormData =
    { fromIndex : Int, toIndex : Int }


type State
    = Loading
    | Failed
    | Loaded Int FormData
    | LoadedWithLogs Int Logs


type alias Logs =
    List Log


type alias Log =
    { index : Int
    , msg : String
    , createdAt : String
    }


apiUrl : String
apiUrl =
    "https://log-hub-6cua55maua-lz.a.run.app"



-- UPDATE


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (Debug.log "url" url) Loading, getIndex )


type Msg
    = GotIndex (Result Http.Error Int)
    | GotLogs (Result Http.Error ResponseWithLogs)
    | FormDataChanged FormData
    | GetEventsClicked FormData
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIndex res ->
            case res of
                Ok index ->
                    ( { model | state = Loaded index { fromIndex = 1, toIndex = index } }, Cmd.none )

                Err err ->
                    let
                        debug =
                            Debug.log "err" err
                    in
                    ( { model | state = Failed }, Cmd.none )

        GotLogs res ->
            case res of
                Ok data ->
                    ( { model | state = LoadedWithLogs data.latestIndex data.events }, Cmd.none )

                Err err ->
                    let
                        debug =
                            Debug.log "err" err
                    in
                    ( { model | state = Failed }, Cmd.none )

        FormDataChanged formData ->
            let
                index =
                    indexFromState model.state
            in
            ( { model | state = Loaded index formData }
            , Cmd.none
            )

        GetEventsClicked formData ->
            ( { model | state = Loading }
            , getLogs formData.fromIndex formData.toIndex
            )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


indexFromState : State -> Int
indexFromState state =
    case state of
        Loading ->
            0

        Failed ->
            0

        Loaded index _ ->
            index

        LoadedWithLogs index _ ->
            index



-- COMMANDS


getLogs : Int -> Int -> Cmd Msg
getLogs from to =
    Http.get
        { expect = Http.expectJson GotLogs logsDecoder
        , url =
            apiUrl
                ++ "/"
                ++ String.fromInt from
                ++ "/"
                ++ String.fromInt to
        }


getIndex : Cmd Msg
getIndex =
    Http.get { expect = Http.expectJson GotIndex indexDecoder, url = apiUrl }


indexDecoder : Decoder Int
indexDecoder =
    field "latest_index" Decode.int


type alias ResponseWithLogs =
    { latestIndex : Int
    , events : Logs
    }


logsDecoder : Decoder ResponseWithLogs
logsDecoder =
    Decode.map2 ResponseWithLogs
        (field "latest_index" Decode.int)
        (field "events" (Decode.list logDecoder))


logDecoder : Decoder Log
logDecoder =
    Decode.map3 Log
        (field "index" Decode.int)
        (field "message" Decode.string)
        (field "created_at" Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        case model.state of
            Loading ->
                [ loadingView ]

            Failed ->
                [ errorView ]

            Loaded index form ->
                [ loadedView index form ]

            LoadedWithLogs index logs ->
                [ loadedWithLogsView index logs ]
    }


loadingView : Html Msg
loadingView =
    div [] [ text "loading.." ]


errorView : Html Msg
errorView =
    div [] [ text "something went wrong" ]


loadedView : Int -> FormData -> Html Msg
loadedView index form =
    div []
        [ text <| "Latest event index is " ++ String.fromInt index ++ "."
        , getLogsInputView index form
        ]


getLogsInputView : Int -> FormData -> Html Msg
getLogsInputView index formD =
    div []
        [ form []
            [ input [ type_ "number", value <| String.fromInt formD.fromIndex, onInput <| firstChanged formD ] []
            , input [ type_ "number", value <| String.fromInt formD.toIndex, onInput <| lastChanged formD ] []
            , input [ type_ "button", value "Get these indexes", onClick <| GetEventsClicked formD ] []
            ]
        ]


firstChanged : FormData -> String -> Msg
firstChanged fdata new =
    let
        newInt =
            Maybe.withDefault fdata.fromIndex (String.toInt new)
    in
    if newInt > 0 && newInt <= fdata.toIndex then
        FormDataChanged { fdata | fromIndex = newInt }

    else
        FormDataChanged fdata


lastChanged : FormData -> String -> Msg
lastChanged fdata new =
    let
        newInt =
            Maybe.withDefault fdata.toIndex (String.toInt new)
    in
    if newInt >= fdata.fromIndex then
        FormDataChanged { fdata | toIndex = newInt }

    else
        FormDataChanged fdata


loadedWithLogsView : Int -> Logs -> Html Msg
loadedWithLogsView index logs =
    div []
        [ text <| "Latest event index is " ++ String.fromInt index ++ "."
        , p []
            [ text "Fetched "
            , text <| String.fromInt <| List.length logs
            , text " forms."
            ]
        , logsListView logs
        ]


logsListView : Logs -> Html Msg
logsListView logs =
    ul [] <|
        List.map (\log -> li [] [ text <| Debug.toString log ]) logs



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
