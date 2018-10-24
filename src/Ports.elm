port module Ports exposing (openInBrowser, openNewWindow)


port openInBrowser : String -> Cmd msg


port openNewWindow : String -> Cmd msg
