module SimpleForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    String


type Msg
    = FormSubmited


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    "" ! []


update msg model =
    case msg of
        FormSubmited ->
            "submited" ! []


view model =
    div []
        [ Html.form
            [ onSubmit FormSubmited
            , action "fileUpload.php"
            , enctype "multipart/form-data"
            , method "post"
            ]
            [ input [ type_ "file" ] []
            , button [ type_ "submit" ]
                [ text "submit" ]
            ]
        , text model
        ]


subscriptions model =
    Sub.batch []
