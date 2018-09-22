module CustomElementUpload exposing (..)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { id : String
    , mFile : Maybe File
    , debug : Maybe Decode.Value
    }


type alias File =
    { contents : String
    , filename : String
    }


type Msg
    = UploadFile
    | FileRead FileData
    | UploadResult (Result Error ())


type alias FileData =
    { contents : String
    , filename : String
    }


decodeFileData =
    Decode.at [ "target", "fileData" ]
        (Decode.map2 FileData
            (Decode.field "contents" Decode.string)
            (Decode.field "filename" Decode.string)
            |> Decode.map FileRead
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { id = "InputId"
      , mFile = Nothing
      , debug = Nothing
      }
    , Cmd.none
    )


update msg model =
    case msg of
        UploadFile ->
            case model.mFile of
                Nothing ->
                    ( model, Cmd.none )

                Just file ->
                    ( model, uploadFile file )

        FileRead data ->
            let
                newFile =
                    { contents = data.contents
                    , filename = data.filename
                    }
            in
                ( { model | mFile = Just newFile }, Cmd.none )

        UploadResult (Ok ()) ->
            ( model, Cmd.none )

        UploadResult (Err e) ->
            ( model, Cmd.none )


view model =
    div []
        [ div
            []
            [ fileReader
                [ id model.id
                , on "fileRead"
                    decodeFileData
                ]
            , button [ onClick UploadFile ]
                [ text "submit" ]
            , text <| Debug.toString model
            ]
        ]


fileReader attributes =
    node "file-reader"
        attributes
        [ Html.input
            [ type_ "file"
            ]
            []
        ]


subscriptions model =
    Sub.batch []


uploadFile : File -> Cmd Msg
uploadFile file =
    Http.send UploadResult (fileUploadRequest file)


fileUploadRequest : File -> Http.Request ()
fileUploadRequest { contents, filename } =
    let
        body =
            Encode.object
                [ ( "contents", Encode.string contents )
                , ( "filename", Encode.string filename )
                ]
    in
        Http.post "fileUpload.php" (jsonBody body) (Decode.succeed ())
