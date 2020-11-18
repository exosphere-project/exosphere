module AppUrl.Parser exposing (urlToViewState)

import Dict
import OpenStack.Types as OSTypes
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( JetstreamCreds
        , JetstreamProvider(..)
        , Model
        , NonProjectViewConstructor(..)
        , ProjectIdentifier
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Url
import Url.Parser exposing (..)
import Url.Parser.Query as Query


urlToViewState : Url.Url -> Maybe ViewState
urlToViewState url =
    parse (oneOf pathParsers) url


pathParsers : List (Parser (ViewState -> b) b)
pathParsers =
    [ -- Non-project-specific views
      map
        (\creds -> NonProjectView <| LoginOpenstack creds)
        (let
            queryParser =
                Query.map6
                    OSTypes.OpenstackLogin
                    (Query.string "authurl"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "pdomain"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "pname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "udomain"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "uname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    -- This parses into a blank password, ugly I know
                    (Query.string ""
                        |> Query.map (\_ -> "")
                    )
         in
         s "login" </> s "openstack" <?> queryParser
        )
    , map
        (\creds -> NonProjectView <| LoginJetstream creds)
        (let
            providerEnumDict =
                Dict.fromList
                    [ ( "iu", IUCloud )
                    , ( "tacc", TACCCloud )
                    , ( "both", BothJetstreamClouds )
                    ]

            queryParser =
                Query.map4
                    JetstreamCreds
                    (Query.enum "provider" providerEnumDict
                        |> Query.map (Maybe.withDefault BothJetstreamClouds)
                    )
                    (Query.string "pname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "taccuname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    -- This parses into a blank password, ugly I know
                    (Query.string ""
                        |> Query.map (\_ -> "")
                    )
         in
         s "login" </> s "jetstream" <?> queryParser
        )
    , map
        (NonProjectView LoginPicker)
        (s "login")

    -- Not bothering to decode the SelectProjects view, because you can't currently navigate there on a fresh page load and see anything useful
    , map
        (NonProjectView MessageLog)
        (s "msglog")
    , map
        (NonProjectView HelpAbout)
        (s "helpabout")

    -- Project-specific views
    , map
        (\uuid -> ProjectView uuid { createPopup = False } <| ListImages Defaults.imageListViewParams Defaults.sortTableParams)
        (s "projects" </> string </> s "images")
    , map
        (\uuid ->
            ProjectView uuid { createPopup = False } <|
                ListProjectServers Defaults.serverListViewParams
        )
        (s "projects" </> string </> s "servers")
    , map
        (\uuid ->
            ProjectView uuid { createPopup = False } <|
                ListProjectVolumes []
        )
        (s "projects" </> string </> s "volumes")
    , map
        (\uuid ->
            ProjectView uuid { createPopup = False } <|
                ListQuotaUsage
        )
        (s "projects" </> string </> s "quotausage")
    , map
        (\projUuid svrUuid ->
            ProjectView projUuid { createPopup = False } <|
                ServerDetail svrUuid Defaults.serverDetailViewParams
        )
        (s "projects" </> string </> s "servers" </> string)
    , map
        (\projUuid svrUuid imageName ->
            ProjectView projUuid { createPopup = False } <|
                CreateServerImage svrUuid imageName
        )
        (let
            queryParser =
                Query.string "name"
                    |> Query.map (Maybe.withDefault "")
         in
         s "projects" </> string </> s "servers" </> string </> s "image" <?> queryParser
        )
    , map
        (\projUuid volUuid ->
            ProjectView projUuid { createPopup = False } <|
                VolumeDetail volUuid []
        )
        (s "projects" </> string </> s "volumes" </> string)
    , map
        (\projUuid params ->
            ProjectView projUuid { createPopup = False } <|
                CreateServer params
        )
        (let
            maybeBoolEnumDict =
                Dict.fromList
                    [ ( "justtrue", Just True )
                    , ( "justfalse", Just False )
                    , ( "nothing", Nothing )
                    ]

            queryParser =
                Query.map3
                    Defaults.createServerViewParams
                    (Query.string "imageuuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "imagename"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.enum "deployguac" maybeBoolEnumDict
                        |> Query.map (Maybe.withDefault Nothing)
                    )
         in
         s "projects" </> string </> s "createserver" <?> queryParser
        )
    , map
        (\projUuid ->
            ProjectView projUuid { createPopup = False } <|
                Defaults.createVolumeView
        )
        (s "projects" </> string </> s "createvolume")
    , map
        (\projUuid ( maybeServerUuid, maybeVolUuid ) ->
            ProjectView projUuid { createPopup = False } <|
                AttachVolumeModal maybeServerUuid maybeVolUuid
        )
        (let
            queryParser =
                Query.map2
                    Tuple.pair
                    (Query.string "serveruuid")
                    (Query.string "voluuid")
         in
         s "projects" </> string </> s "attachvol" <?> queryParser
        )
    , map
        (\projUuid attachment ->
            ProjectView projUuid { createPopup = False } <|
                MountVolInstructions attachment
        )
        (let
            queryParser =
                Query.map3
                    OSTypes.VolumeAttachment
                    (Query.string "serveruuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "attachmentuuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "device"
                        |> Query.map (Maybe.withDefault "")
                    )
         in
         s "projects" </> string </> s "attachvolinstructions" <?> queryParser
        )
    ]
