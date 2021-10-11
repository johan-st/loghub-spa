module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field)
import String exposing (fromInt)
import Url



-- MODEL


type alias Flags =
    { apiUrl : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : State
    , apiUrl : String
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



-- UPDATE


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Loading flags.apiUrl, getIndex flags.apiUrl )


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
                Ok latestIndex ->
                    ( { model | state = Loaded latestIndex { fromIndex = 1, toIndex = latestIndex } }, Cmd.none )

                Err _ ->
                    ( { model | state = Failed }, Cmd.none )

        GotLogs res ->
            case res of
                Ok data ->
                    ( { model | state = LoadedWithLogs data.latestIndex data.events }, Cmd.none )

                Err _ ->
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
            , getLogs model.apiUrl formData.fromIndex formData.toIndex
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


getLogs : String -> Int -> Int -> Cmd Msg
getLogs apiUrl from to =
    Http.get
        { expect = Http.expectJson GotLogs logsDecoder
        , url =
            apiUrl
                ++ "/"
                ++ String.fromInt from
                ++ "/"
                ++ String.fromInt to
        }


getIndex : String -> Cmd Msg
getIndex apiUrl =
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


inlineCss : List (Attribute Msg)
inlineCss =
    [ style "font-family" "sans-serif"
    , style "background-color" "#333"
    , style "color" "#f90"
    , style "min-height" "100vh"
    ]


loadingView : Html Msg
loadingView =
    div (inlineCss ++ []) [ text "loading.." ]


errorView : Html Msg
errorView =
    div (inlineCss ++ []) [ text "something went wrong" ]


loadedView : Int -> FormData -> Html Msg
loadedView index form =
    div (inlineCss ++ [])
        [ text <| "Latest event index is " ++ String.fromInt index ++ "."
        , getLogsInputView index form
        ]


getLogsInputView : Int -> FormData -> Html Msg
getLogsInputView index formD =
    div (inlineCss ++ [])
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
    div (inlineCss ++ [])
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
        List.map
            (\log ->
                div []
                    [ li [] [ text <| logToString log ] ]
            )
            logs


logToString : Log -> String
logToString l =
    "{ "
        ++ "createdAt= \""
        ++ l.createdAt
        ++ "\", index= "
        ++ String.fromInt l.index
        ++ ", msg= \""
        ++ l.msg
        ++ "\" }"



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
