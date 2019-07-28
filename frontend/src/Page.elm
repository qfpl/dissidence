module Page exposing (ParentMsg(..), SubMsg(..), logoutView, wrapChildMsg, wrapParentMsg)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Route
import Session


type ParentMsg
    = SetPlayer Route.Route (Maybe Session.Player)
    | Logout


{-| Messages that child pages raise to here.
-}
type SubMsg a
    = ParentMsg ParentMsg -- A message that are global and handled by the top level update
    | ChildMsg a -- A message that is just for the child's update


wrapParentMsg : (b -> ParentMsg) -> b -> SubMsg a
wrapParentMsg f =
    f >> ParentMsg


wrapChildMsg : (b -> a) -> b -> SubMsg a
wrapChildMsg f =
    f >> ChildMsg


logoutView : Session.Player -> H.Html (SubMsg a)
logoutView player =
    H.p [ HA.class "logged-in" ]
        [ H.text ("Logged in as " ++ player.playerId ++ ".")
        , H.button [ HA.class "logout", HE.onClick (ParentMsg Logout) ] [ H.text "logout" ]
        ]
