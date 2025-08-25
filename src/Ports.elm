port module Ports exposing
    ( changeThemePreference
    , instantiateClipboardJs
    , logout
    , openNewWindow
    , pushUrlAndTitleToMatomo
    , receiveWebLock
    , releaseWebLock
    , requestWebLock
    , setFavicon
    , setStorage
    , updateNetworkConnectivity
    )

import Json.Encode as Encode


port changeThemePreference : (Encode.Value -> msg) -> Sub msg


{-| Port for receiving offline/online events from the browser.

    ref. https://developer.mozilla.org/en-US/docs/Web/API/Window/offline_event

-}
port updateNetworkConnectivity : (Bool -> msg) -> Sub msg


port openNewWindow : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


port instantiateClipboardJs : () -> Cmd msg


port logout : () -> Cmd msg


port setFavicon : String -> Cmd msg


port pushUrlAndTitleToMatomo : { newUrl : String, pageTitle : String } -> Cmd msg


{-| Request a web lock to synchronise resource access.
-}
port requestWebLock : String -> Cmd msg


{-| Notification of whether a web lock has been granted.
-}
port receiveWebLock : (( String, Bool ) -> msg) -> Sub msg


{-| Release a previously acquired web lock.
-}
port releaseWebLock : String -> Cmd msg
