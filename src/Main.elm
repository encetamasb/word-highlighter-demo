module Main exposing (..)

import Browser
import Debug
import Html exposing (Attribute, Html, audio, div, text)
import Html.Attributes exposing (class, controls, id, src, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { mediaUrl : String
    , transcriptDataUrl : String
    , mediaType : String
    , currentTime : Float
    , transcripts : Maybe (List Transcript)
    }


type alias WordChunk =
    { stime : Float
    , duration : Float
    , content : String
    , confidence : Float
    }


type alias TranscriptData =
    { status : String
    , task_id : String
    , task_url : String
    , task_result : TranscriptTaskResult
    }


type alias TranscriptTaskResult =
    { engine : String
    , language : String
    , filename : String
    , transcripts : List Transcript
    }


type alias Transcript =
    { speaker : String
    , stime : Float
    , duration : Float
    , content : String
    , word_chunks : List WordChunk
    , bookmarks : String
    }



-- MSG


type Msg
    = NoOp
    | TimeUpdate Float
    | GotTranscriptData (Result Http.Error (List TranscriptData))



-- INIT


init : String -> ( Model, Cmd Msg )
init base_url =
    let
        model =
            { mediaUrl = base_url ++ "/Test4.wav"
            , transcriptDataUrl = base_url ++ "/test4_transcript.json_out.json"
            , mediaType = "audio/wav"
            , currentTime = 0.0
            , transcripts = Nothing
            }
    in
    ( model
    , getTranscriptData model.transcriptDataUrl
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate t ->
            ( { model | currentTime = t }, Cmd.none )

        GotTranscriptData r ->
            case r of
                Ok (data :: xs) ->
                    ( { model | transcripts = Just data.task_result.transcripts }, Cmd.none )

                Ok [] ->
                    ( model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getCurrentTranscript : List Transcript -> Float -> Maybe Transcript
getCurrentTranscript transcripts time =
    case transcripts of
        [] ->
            Nothing

        trans :: remaining ->
            if trans.stime <= time && trans.stime + trans.duration > time then
                Just trans

            else
                getCurrentTranscript remaining time


getCurrentWordChunk : List WordChunk -> Float -> Maybe WordChunk
getCurrentWordChunk wchunks time =
    case wchunks of
        [] ->
            Nothing

        chunk :: remaining ->
            if chunk.stime <= time && chunk.stime + chunk.duration > time then
                Just chunk

            else
                getCurrentWordChunk remaining time


view : Model -> Html Msg
view model =
    let
        currTime =
            model.currentTime + 1

        transcript =
            getCurrentTranscript (Maybe.withDefault [] model.transcripts) currTime

        speaker =
            Maybe.map (\t -> t.speaker) transcript

        wchunk =
            transcript
                |> Maybe.andThen
                    (\t ->
                        getCurrentWordChunk t.word_chunks currTime
                    )
    in
    div [ class "elm-audio-player" ]
        [ audio
            [ src model.mediaUrl
            , id "audio-player"
            , type_ model.mediaType
            , controls True
            , onTimeUpdate TimeUpdate
            ]
            []
        , div [] [ text << String.fromFloat <| model.currentTime ]
        , currentWordView speaker wchunk
        ]


currentWordView speaker wchunk =
    case ( speaker, wchunk ) of
        ( Just sp, Just ch ) ->
            div []
                [ div [] [ text <| "(" ++ sp ++ ")" ]
                , div [] [ text ch.content ]
                ]

        _ ->
            text ""


onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    on "timeupdate" (Decode.map msg targetCurrentTime)


targetCurrentTime : Decoder Float
targetCurrentTime =
    Decode.at [ "target", "currentTime" ] Decode.float


transcriptDataDecoder : Decoder TranscriptData
transcriptDataDecoder =
    Decode.succeed TranscriptData
        |> required "status" Decode.string
        |> required "task_id" Decode.string
        |> required "task_url" Decode.string
        |> required "task_result" transcriptTaskResultDecoder


transcriptTaskResultDecoder : Decoder TranscriptTaskResult
transcriptTaskResultDecoder =
    Decode.succeed TranscriptTaskResult
        |> required "engine" Decode.string
        |> required "language" Decode.string
        |> required "filename" Decode.string
        |> required "transcripts" (Decode.list transcriptDecoder)


transcriptDecoder : Decoder Transcript
transcriptDecoder =
    Decode.succeed Transcript
        |> required "speaker" Decode.string
        |> required "stime" Decode.float
        |> required "duration" Decode.float
        |> required "content" Decode.string
        |> required "word_chunks" (Decode.list wordChunkDecoder)
        |> required "bookmarks" Decode.string


wordChunkDecoder : Decoder WordChunk
wordChunkDecoder =
    Decode.succeed WordChunk
        |> required "stime" Decode.float
        |> required "duration" Decode.float
        |> required "content" Decode.string
        |> required "confidence" Decode.float


getTranscriptData url =
    Http.get
        { url = url
        , expect = Http.expectJson GotTranscriptData (Decode.list transcriptDataDecoder)
        }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }