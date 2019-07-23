module Page.Lobby exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import List.Nonempty as NEL
import Page
import Ports exposing (putUserSession)
import RemoteData exposing (RemoteData)
import Result
import Route
import Session
import Time
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetNewLine String
    | Tick Time.Posix
    | Submit
    | HandleNewLineResp (Result Http.Error ())
    | HandleListResp (Result Http.Error (List BE.ChatLine))


type alias Model =
    { lastUpdated : Maybe Time.Posix
    , newChatLine : String
    , chatLines : List BE.ChatLine
    , chatListError : Maybe String
    , validationIssues : List String
    , submission : RemoteData String ()
    }


type alias PageMsg =
    Page.SubMsg Msg


init : Nav.Key -> Session.User -> ( Model, Cmd PageMsg )
init key user =
    ( { newChatLine = ""
      , lastUpdated = Nothing
      , chatLines = []
      , chatListError = Nothing
      , validationIssues = []
      , submission = RemoteData.NotAsked
      }
    , BE.getApiLobby user.token Nothing (HandleListResp >> Page.ChildMsg)
    )


subscriptions : Session.User -> Model -> Sub PageMsg
subscriptions _ _ =
    Time.every 2000 (Page.wrapChildMsg Tick)


update : Nav.Key -> Session.User -> Msg -> Model -> ( Model, Cmd PageMsg )
update key user msg model =
    case msg of
        SetNewLine l ->
            ( { model | newChatLine = l }, Cmd.none )

        Submit ->
            case validateNewChatLine user model of
                Ok newChatLine ->
                    ( { model | validationIssues = [], submission = RemoteData.Loading }
                    , BE.postApiLobby user.token newChatLine (Page.wrapChildMsg HandleNewLineResp)
                    )

                Err problems ->
                    ( { model
                        | submission = RemoteData.NotAsked
                        , validationIssues = problems
                      }
                    , Cmd.none
                    )

        Tick t ->
            ( model, BE.getApiLobby user.token Nothing (HandleListResp >> Page.ChildMsg) )

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
            ( { model | submission = remoteData, newChatLine = "" }
            , RemoteData.unwrap
                Cmd.none
                (\us -> BE.getApiLobby user.token Nothing (Page.wrapChildMsg HandleListResp))
                remoteData
            )



-- elm-verify is a much better way of doing this. But this is our only validation.
-- Come back to this later.


validateNewChatLine : Session.User -> Model -> Result.Result (List String) BE.NewChatLine
validateNewChatLine user model =
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
        Result.Ok { newChatLineUsername = user.username, newChatLineText = trimmedLine, newChatGameId = Nothing }

    else
        Result.Err allErrs


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence: Compositional Crusaders - Register"
    , body =
        [ H.div [ HA.class "chatbox-container" ]
            [ H.h1 [] [ H.text "Lobby" ]
            , H.div [ HA.class "chatbox" ] (List.map chatLineView model.chatLines)
            , H.form [ HE.onSubmit Submit ]
                [ H.ul []
                    [ H.li [ HA.class "chat-message" ]
                        [ H.input
                            [ HA.placeholder "type a chat message"
                            , HE.onInput SetNewLine
                            , HA.value model.newChatLine
                            , HA.class "chat-message-input"
                            , HAA.ariaLabel "Enter Chat Message"
                            ]
                            []
                        ]
                    , H.li []
                        [ H.button
                            [ HA.class "btn primary", disabledIfLoading model.submission ]
                            [ H.text "send" ]
                        ]
                    , let
                        errorsMay =
                            NEL.fromList (maybeToList (remoteDataError model.submission) ++ model.validationIssues)
                      in
                      Utils.maybe (H.text "") chatWarnings errorsMay
                    ]
                ]
            ]
        ]
    }


chatWarnings : NEL.Nonempty String -> H.Html Msg
chatWarnings errors =
    H.li [ HA.class "chat-warnings" ] [ H.ul [ HA.class "warn" ] (List.map (\em -> H.li [] [ H.text em ]) (NEL.toList errors)) ]


chatLineView : BE.ChatLine -> H.Html Msg
chatLineView cl =
    H.p []
        [ H.b [] [ H.text cl.chatLineUsername, H.text "> " ]
        , H.text cl.chatLineText
        ]
