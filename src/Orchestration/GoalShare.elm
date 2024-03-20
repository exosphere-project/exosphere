module Orchestration.GoalShare exposing (goalNewShare)

import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP exposing (Haveness(..), RefreshStatus(..))
import List
import OpenStack.Shares as Shares
import OpenStack.Types exposing (AccessRuleAccessLevel(..), AccessRuleAccessType(..), Share, ShareStatus(..), accessRuleAccessLevelToApiString)
import Orchestration.Helpers exposing (applyProjectStep, pollRDPP)
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg)
import UUID


goalNewShare : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalNewShare exoClientUuid time project =
    let
        steps =
            [ stepPollNewShares time
            , stepNewShareAccessRule exoClientUuid time
            ]
    in
    List.foldl
        applyProjectStep
        ( project, Cmd.none )
        steps


stepPollNewShares : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepPollNewShares time project =
    if
        anyShareIsNewAndHasNoAccessRules time project
            && pollRDPP project.shares time 5000
    then
        case project.endpoints.manila of
            Just url ->
                ( GetterSetters.projectSetSharesLoading project
                , Shares.requestShares project url
                )

            Nothing ->
                ( project, Cmd.none )

    else
        ( project, Cmd.none )


stepNewShareAccessRule : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepNewShareAccessRule exoClientUuid time project =
    RDPP.withDefault [] project.shares
        |> List.filter (shareIsNew time)
        -- Don't create a duplicate access rule from another client if they happen to load at the same time.
        |> List.filter (shareIsFromThisExoClient exoClientUuid)
        |> List.map
            (createNewShareAccessRule project)
        |> List.foldl
            -- Accumulate the commands into a batch & the loading states into the project.
            (\( stepProject, newCmd ) ( _, cmds ) -> ( stepProject, Cmd.batch [ cmds, newCmd ] ))
            ( project, Cmd.none )


createNewShareAccessRule : Project -> Share -> ( Project, Cmd SharedMsg )
createNewShareAccessRule project share =
    if
        shareIsAvailable share
            && shareHasNoAccessRules project share
    then
        -- Create a default access rule for a new share.
        case project.endpoints.manila of
            Just manilaUrl ->
                let
                    defaultAccessLevel =
                        RW
                in
                ( GetterSetters.projectSetShareAccessRulesLoading share.uuid project
                , Shares.requestCreateAccessRule project
                    manilaUrl
                    { shareUuid = share.uuid
                    , accessLevel = defaultAccessLevel
                    , accessType = CephX
                    , accessTo = String.join "-" [ Maybe.withDefault share.uuid share.name, accessRuleAccessLevelToApiString defaultAccessLevel ]
                    }
                )

            Nothing ->
                ( project, Cmd.none )

    else
        ( project, Cmd.none )


shareIsAvailable : Share -> Bool
shareIsAvailable share =
    share.status == ShareAvailable


anyShareIsNewAndHasNoAccessRules : Time.Posix -> Project -> Bool
anyShareIsNewAndHasNoAccessRules time project =
    RDPP.withDefault [] project.shares
        |> List.any
            (\share ->
                shareIsNew time share && shareHasNoAccessRules project share
            )


shareIsNew : Time.Posix -> Share -> Bool
shareIsNew time share =
    let
        oneMinuteOfMillis =
            60 * 1000

        ageInMillis =
            Time.posixToMillis time
                - Time.posixToMillis share.createdAt
    in
    ageInMillis < oneMinuteOfMillis


shareIsFromThisExoClient : UUID.UUID -> Share -> Bool
shareIsFromThisExoClient exoClientUuid share =
    -- Shares are created with `exoClientUuid` in the metadata to track the client.
    case Dict.get "exoClientUuid" share.metadata of
        Just shareExoClientUuid ->
            shareExoClientUuid == UUID.toString exoClientUuid

        Nothing ->
            False


shareHasNoAccessRules : Project -> Share -> Bool
shareHasNoAccessRules project share =
    case Dict.get share.uuid project.shareAccessRules of
        Just accessRulesRDPP ->
            case ( accessRulesRDPP.refreshStatus, accessRulesRDPP.data ) of
                ( NotLoading _, DoHave accessRules _ ) ->
                    List.length accessRules == 0

                _ ->
                    -- This loading state debounces create access rule requests.
                    False

        Nothing ->
            -- Share access rules are loaded lazily, so we can't assume there are none if the value is missing in the dictionary.
            False
