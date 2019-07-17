module Page.Lobby exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Ports exposing (putUserSession)
import RemoteData exposing (RemoteData)
import Result
import Route
import Session
import Time
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetNewLine String
    | Submit
    | HandleNewLineResp (Result Http.Error ())
    | HandleListResp (Result Http.Error (List BE.ChatLine))


type alias Model =
    { key : Nav.Key
    , user : Session.User
    , lastUpdated : Maybe Time.Posix
    , newChatLine : String
    , chatLines : List BE.ChatLine
    , chatListError : Maybe String
    , validationIssues : List String
    , submission : RemoteData String ()
    }


init : Nav.Key -> Session.User -> ( Model, Cmd Msg )
init key user =
    ( { key = key
      , user = user
      , newChatLine = ""
      , lastUpdated = Nothing
      , chatLines = []
      , chatListError = Nothing
      , validationIssues = []
      , submission = RemoteData.NotAsked
      }
    , BE.getApiLobby Nothing HandleListResp
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewLine l ->
            ( { model | newChatLine = l }, Cmd.none )

        Submit ->
            case validateNewChatLine model of
                Ok newChatLine ->
                    ( { model | validationIssues = [], submission = RemoteData.Loading }
                    , BE.postApiLobby newChatLine HandleNewLineResp
                    )

                Err problems ->
                    ( { model
                        | submission = RemoteData.NotAsked
                        , validationIssues = problems
                      }
                    , Cmd.none
                    )

        HandleListResp (Err e) ->
            ( { model | chatListError = Just (Utils.httpErrorToStr e) }, Cmd.none )

        HandleListResp (Ok l) ->
            ( { model | chatListError = Nothing, chatLines = l }, Cmd.none )

        HandleNewLineResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
            in
            ( { model | submission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\us -> BE.getApiLobby Nothing HandleListResp)
                remoteData
            )



-- elm-verify is a much better way of doing this. But this is our only validation.
-- Come back to this later.


validateNewChatLine : Model -> Result.Result (List String) BE.NewChatLine
validateNewChatLine model =
    let
        trimmedLine =
            String.trim model.newChatLine

        newChatLineError =
            if trimmedLine == "" then
                [ "Message cannot be blank" ]

            else
                []

        allErrs =
            List.concat [ newChatLineError ]
    in
    if allErrs == [] then
        Result.Ok { newChatLineUsername = model.user.username, newChatLineText = trimmedLine, newChatGameId = Nothing }

    else
        Result.Err allErrs


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence: Compositional Crusaders - Register"
    , body =
        [ H.div []
            [ H.h1 [] [ H.text "Lobby" ]
            , H.div [ HA.class "chatbox" ] (List.map chatLineView model.chatLines)
            , H.form [ HA.class "", HE.onSubmit Submit ]
                [ H.input
                    [ HA.placeholder "type a chat message"
                    , HE.onInput SetNewLine
                    , HA.value model.newChatLine
                    ]
                    []
                , H.button
                    [ HA.class "btn primary", disabledIfLoading model.submission ]
                    [ H.text "send" ]
                , H.ul [ HA.class "warn" ]
                    (model.submission
                        |> remoteDataError
                        |> maybeToList
                        |> List.append model.validationIssues
                        |> List.map (\em -> H.li [] [ H.text em ])
                    )
                ]
            ]
        ]
    }


chatLineView : BE.ChatLine -> H.Html Msg
chatLineView cl =
    H.p [] [ H.text cl.chatLineUsername, H.text "> ", H.text cl.chatLineText ]
