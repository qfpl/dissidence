module Page exposing (ParentMsg(..), SubMsg(..), wrapChildMsg, wrapParentMsg)

import Session


type ParentMsg
    = SetUser (Maybe Session.User)


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
