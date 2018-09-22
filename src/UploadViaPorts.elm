port module UploadViaPorts exposing (..)

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
    }


type alias File =
    { contents : String
    , filename : String
    }


type Msg
    = FormSubmited
    | FileSelected
    | FileRead FilePortData
    | UploadResult (Result Error ())


type alias FilePortData =
    { contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg


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
      }
    , Cmd.none
    )


update msg model =
    case msg of
        FormSubmited ->
            case model.mFile of
                Nothing ->
                    ( model, Cmd.none )

                Just file ->
                    ( model, Cmd.batch [ uploadFile file ] )

        FileSelected ->
            ( model, Cmd.batch [ fileSelected model.id ] )

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
        [ Html.form
            [ onSubmit FormSubmited
            ]
            [ input
                [ type_ "file"
                , id model.id
                , on "change"
                    (Decode.succeed FileSelected)
                ]
                []
            , button [ type_ "submit" ]
                [ text "submit" ]
            ]
        ]


subscriptions model =
    Sub.batch [ fileContentRead FileRead ]


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
