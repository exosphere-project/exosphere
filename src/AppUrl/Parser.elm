module AppUrl.Parser exposing (urlToViewState)

import Dict
import OpenStack.Types as OSTypes
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
    let
        pathParsers =
            [ map
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
            ]
    in
    parse (oneOf pathParsers) url
