module AppUrl.Parser exposing (urlToViewState)

import OpenStack.Types as OSTypes
import Types.Defaults as DefaultTypes
import Types.Types
    exposing
        ( JetstreamProvider(..)
        , Model
        , NonProjectViewConstructor(..)
        , ProjectIdentifier
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Url
import Url.Parser as P exposing (..)


urlToViewState : Url.Url -> Maybe ViewState
urlToViewState url =
    -- TODO parser will need to know path prefix on try, try-dev, etc
    let
        pathParsers =
            [ map
                -- TODO parse query string here
                (NonProjectView <| LoginOpenstack <| OSTypes.OpenstackLogin "" "" "" "" "" "")
                (s "login" </> s "openstack")
            , map
                -- TODO parse query string here
                (NonProjectView <| LoginJetstream <| DefaultTypes.jetstreamCreds)
                (s "login" </> s "jetstream")
            , map
                (NonProjectView LoginPicker)
                (s "login")
            ]
    in
    parse (oneOf pathParsers) url
