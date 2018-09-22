port module Dropzone exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import UserAccounts exposing (..)
import Set exposing (..)
import Http exposing (..)
import Utils exposing (..)
import ComBox exposing (..)

type Msg = LoadDropzone
         | Default
         | ProcessHttpResult (Result Error Res)
         | ChangePos DropPos
         | RemoveFile String
         | RemovePic String
         | RemovePics
         | RemoveFiles
         | UpdatePicsPerm
         | UpdateFilesPerm
         | NewPerm String
         | DropzoneCom Encode.Value
         | ToComBox (ComBox.Msg)
         | Message String MessageKind


type alias Model = 
  { dropPos : DropPos
  , pics : List (String,String,Int)
  , files : List (String, String,Int)
  , picsToChange : List String
  , filesToChange : List String
  , usrType : String 
  , combox : ComBox.Model
  }

type alias DropZoneVal = 
  { pos : String
  , status : String
  }

type Res = Pictures (List (String,String,Int))
         | Files (List (String,String,Int))
         | ServerCom String 
         | ServerError String
         | Refresh (String, String)
         

type DropPos = FilesPos | PicsPos

init = Model FilesPos
             []
             []
             []
             []
             "1"
             ComBox.init ! []

fileView model (fn,tn,ut) = 
  div [class "fileViewContainer"] [
  div [ class "fileView"
      , classList 
          [("selected"
           , List.member tn 
                         (case .dropPos model of 
                            FilesPos -> .filesToChange model
                            PicsPos -> .picsToChange model 
                         ) 
          )]

      , case .dropPos model of 
          FilesPos -> onClick (RemoveFile tn)
          PicsPos -> onClick (RemovePic tn) 
      ]
      [ span [class "filename"] [text fn]
      
      , span [class "usrType"] [text <| toString ut]
      ]
      , a [ class "downloadLink"
          , downloadAs fn
          , case .dropPos model of 
              FilesPos -> href <| domainAdr ("showFile.php?filename=" ++ fn)
              PicsPos -> href <| domainAdr ("showPic.php?filename=" ++ fn)
          ]
          [text "⇩"]
      ]

view model = 
    div [id "dropZoneContainer"] [
    div [id "dropZoneView"]
        [ div 
           [ id "dropChooseControls"]
           [ div [ class "dropChangePos"
                 , classList [("selected", .dropPos model == FilesPos)]
                 , onClick (ChangePos FilesPos)
                 ] 
                  [text "Files"]
           , div [ class "dropChangePos"
                 , classList [("selected", .dropPos model == PicsPos)]
                 , onClick (ChangePos PicsPos) 
                 ]
                 [text "Pictures"]
           ]
        
        , div [ id "fileSelector"
              ]
              [case .dropPos model of
                  FilesPos -> 
                    div [id "filebox", class "greyBorder"] <|
                        case List.map (fileView model) (.files model) of
                          [] -> [span [id "defMes"]
                                      [text "No files in remote folder"]
                                ]
                          xs -> xs
                  PicsPos -> 
                    div [id "filebox"] <|
                        case List.map (fileView model) (.pics model) of
                          [] -> [span [id "defMes"]
                                      [text "No pictures in remote folder"]
                                ]
                          xs -> xs  
                
                , button [ case .dropPos model of
                            FilesPos -> onClick RemoveFiles
                            PicsPos -> onClick RemovePics
                         ]
                         [text "Remove from server"]
                
                , div [id "dropUsrType"]
                      [ select [ id "usrTypSelect"
                               , onInput NewPerm
                               ]
                               ([ option [value "1"]
                                         [text "admin"]
                                ]
                                 ++ List.map 
                                     (\n -> option [ value (toString n)
                                                   --, selectedIf ((usrTypToInt typ) == n)
                                                   ]
                                                   [text <| "user class " ++ toString n]
                                     ) (List.range 2 maxUsrTyp)
                              )
                      , button [case .dropPos model of
                                 FilesPos -> onClick UpdateFilesPerm
                                 PicsPos -> onClick UpdatePicsPerm
                               ] 
                               [text "Update permissions"]
                      ]
               ] 
              
        , div [id "dropContainer"]
              ([ case .dropPos model of
                  FilesPos -> text "Drop your files here:"
                  PicsPos -> text "Drop your pictures here:"
              , br [] []]
              ++ (case .dropPos model of
                  FilesPos -> 
                    [ span [] []
                    ,  div [ id "filesDropzone"   
                           , class "dropzone"
                           --, style [("width","70%")]
                           ]
                           []
                    ]
                  PicsPos -> 
                    [ div [ id "picsDropzone"   
                          , class "dropzone"
                          --, style [("width","70%")]
                          ]
                          []
                    , span [] []
                    ]))
              
        ]
        , div [id "dropComboxContainer"]
              [ Html.map ToComBox (ComBox.view (.combox model))
              , a [href "#fs?edit"]         
                  [button [] 
                  [text "Back to editor"]
                  ]
              ]
        ]
      

toJs credentials model = 
  let position = 
        case .dropPos model of 
          FilesPos -> [("position", Encode.string "files")] 
          PicsPos -> [("position", Encode.string "pictures")]
      creds = [("credentials", encodeCred credentials)]
  in Encode.object <| position ++ creds


picsView model = 
  div [id "picsView"]
      []

filesView model = 
  div [id "filesView"]
      []

update msg model credentials = 
    case msg of 
        LoadDropzone -> 
            model ! [ loadDropzone (toJs credentials model)
                    , message "loading remote content info" Com
                    , getPics credentials
                    , getFiles_ credentials
                    ]
        
        Default -> model ! []

        ProcessHttpResult res ->
          case res of 
            Err e -> model ! [message ("JSON decoding error: " ++ toString e) Error]
            Ok (ServerError e) -> 
              model ! [message (toString e) Error] 
            Ok (ServerCom m ) ->
              model ! [message m Com]
            Ok (Pictures pics) -> 
              { model | pics = pics } ! [message "pictures info loaded" Com]
            Ok (Files fs) -> 
              { model | files = fs } ! [message "files info loaded" Com]
            Ok (Refresh (m,t)) -> 
               model ! if t == "files"
                       then [ message ("file(s): " ++ m)  Com
                            , getFiles_ credentials]
                       else [ message ("picture(s): " ++ m) Com
                            , getPics credentials]
                          


        ChangePos pos -> 
          if pos == (.dropPos model)
          then model ! []
          else 
            let newModel = { model | dropPos = pos }
            in 
            newModel ! [loadDropzone (toJs credentials newModel)] 

        RemovePic tn -> 
          { model | picsToChange = adder (.picsToChange model) tn }
          ! []
        RemoveFile tn -> 
          { model | filesToChange = adder (.filesToChange model) tn }
          ! []
        
        RemovePics -> 
          case .picsToChange model of 
            [] -> model ! [message "no picture selected" Error]
            xs -> 
              { model | picsToChange = [] }
              ! [removePics credentials xs]

        RemoveFiles -> 
          case .filesToChange model of 
            [] -> model ! [message "no file selected" Error] 
            xs ->
              { model | filesToChange = [] }
              ! [removeFiles credentials xs]
        
        NewPerm n -> 
          { model | usrType = n }
          ! []
        
        UpdatePicsPerm -> 
          case .picsToChange model of 
            [] -> model ! [message "no picture selected" Error]
            xs ->
              { model | picsToChange = [] }
              ! [uploadPicsPerms (.usrType model) credentials xs]

        UpdateFilesPerm -> 
          case .filesToChange model of 
            [] -> model ! [message "no file selected" Error] 
            xs ->
              { model | filesToChange = [] }
              ! [uploadFilesPerms (.usrType model) credentials xs]
         

        DropzoneCom s -> 
          model  ! dropzoneComCmd s credentials

        ToComBox msg -> 
          let (newComBox, newComBoxFx) = ComBox.update msg (.combox model)
          in  { model | combox = newComBox }
             ! [Cmd.map ToComBox newComBoxFx]

        Message m k -> 
          let (newComBox, newComBoxFx) = 
                 ComBox.update (ComBox.NewMessage m k)
                               (.combox model)
          in { model | combox = newComBox }
             ! [Cmd.map ToComBox newComBoxFx]
             

            

message s k = toCmd <| Message s k


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ dropZoneComPort DropzoneCom ]


port dropZoneComPort : (Encode.Value -> msg ) -> Sub msg


port loadDropzone : Encode.Value -> Cmd msg

decodeDropZomCom = 
  Decode.map2 DropZoneVal
   (Decode.field "pos" Decode.string)
   (Decode.field "status" Decode.string)

dropzoneComCmd json credentials =
  case Decode.decodeValue decodeDropZomCom json of 
    Err e -> [message ("dropzone.js error: " ++ toString e) Error]
    Ok {pos, status} ->
      if status /= "success"
      then [message ("upload failure " ++ status) Error]
      else if pos == "pictures"
           then [ message "picture uploaded" Com
                , getPics credentials]
           else [ message "file uploaded" Com
                , getFiles_ credentials]


decodeServerCom = Decode.map ServerCom
                            (Decode.field "message" Decode.string)

decodeServerError = Decode.map ServerError
                              (Decode.field "serverError" Decode.string)

decodePictures =
  Decode.map Pictures 
    (Decode.field "pictures"
      (Decode.list 
        (Decode.map3 (,,) 
           (Decode.field "picname" Decode.string)
           (Decode.field "tmpname" Decode.string)
           (Decode.field "usrType" Decode.int)
        )
      )
    )


decodeFiles =
  Decode.map Files
    (Decode.field "files"
      (Decode.list 
        (Decode.map3 (,,) 
           (Decode.field "filename" Decode.string)
           (Decode.field "tmpname"  Decode.string)
           (Decode.field "usrType" Decode.int)
        )
      )
    )


decodeRemoval = 
  Decode.map (\(b,t) -> Refresh ("removal complete",t))
   (Decode.map2 (,)
     (Decode.field "Removed" Decode.bool)
     (Decode.field "Type"  Decode.string)
   )

decodeUploadPerm = 
  Decode.map (\(b,t) -> Refresh ("permissions updated",t))
   (Decode.map2 (,)
     (Decode.field "PermUploaded" Decode.bool)
     (Decode.field "Type"  Decode.string)
   )  

decodeRes : Decode.Decoder Res
decodeRes = Decode.oneOf [ decodeServerCom
                         , decodeServerError
                         , decodePictures
                         , decodeFiles
                         , decodeRemoval
                         , decodeUploadPerm
                         ]

getPics : LogInfo -> Cmd Msg
getPics cred = 
  let body = bodyCred cred
      
      request = 
        Http.post (domainAdr "getPictures.php") body decodeRes
  
  in Http.send ProcessHttpResult request

getFiles_ : LogInfo -> Cmd Msg
getFiles_ cred = 
  let body = bodyCred cred
      
      request = 
        Http.post (domainAdr "getFiles.php") body decodeRes
  
  in Http.send ProcessHttpResult request

removePics : LogInfo -> List String -> Cmd Msg
removePics cred xs =
  let content = 
        [ ("picsToRemove", Encode.list <| (List.map Encode.string) xs)
        ]

      body = bodyWithCred cred content

      request = 
        Http.post (domainAdr "removePics.php") body decodeRes
  in Http.send ProcessHttpResult request
  

removeFiles : LogInfo -> List String -> Cmd Msg
removeFiles cred xs =
  let content = 
        [ ("filesToRemove", Encode.list <| (List.map Encode.string) xs)
        ]

      body = bodyWithCred cred content

      request = 
        Http.post (domainAdr "removeFiles.php") body decodeRes
  in Http.send ProcessHttpResult request

uploadPicsPerms : String -> LogInfo -> List String -> Cmd Msg
uploadPicsPerms typ cred xs =
  let content = 
        [ ("picsPermToUpdate", Encode.list <| (List.map Encode.string) xs)
        , ("usrType", Encode.int <| Result.withDefault 1 (String.toInt typ))
        ]

      body = bodyWithCred cred content

      request = 
        Http.post (domainAdr "picsPermUpload.php") body decodeRes
  in Http.send ProcessHttpResult request
  

uploadFilesPerms : String -> LogInfo -> List String -> Cmd Msg
uploadFilesPerms typ cred xs =
  let content = 
        [ ("filesPermToUpdate", Encode.list <| (List.map Encode.string) xs)
        , ("usrType", Encode.int <| Result.withDefault 1 (String.toInt typ)) 
        ]

      body = bodyWithCred cred content

      request = 
        Http.post (domainAdr "filesPermUpload.php") body decodeRes
  in Http.send ProcessHttpResult request


adder xs v = 
  let go xs v acc = 
        case xs of 
          [] -> v::acc
          (x::xs) -> 
            if x == v 
            then xs ++ acc
            else go xs v (x::acc)
  in go xs v []