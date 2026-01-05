module Types.SecurityGroupActions exposing (ComparableSecurityGroupActionId, SecurityGroupAction, SecurityGroupActionId(..), fromComparableSecurityGroupActionId, initPendingRulesChanges, initPendingSecurityGroupChanges, initSecurityGroupAction, toComparableSecurityGroupActionId)

import OpenStack.Types exposing (SecurityGroupUuid, ServerUuid)


type SecurityGroupActionId
    = ExtantGroup SecurityGroupUuid
    | NewGroup String


type alias ComparableSecurityGroupActionId =
    String


toComparableSecurityGroupActionId : SecurityGroupActionId -> ComparableSecurityGroupActionId
toComparableSecurityGroupActionId actionId =
    case actionId of
        ExtantGroup uuid ->
            "extant::" ++ uuid

        NewGroup name ->
            "new::" ++ name


fromComparableSecurityGroupActionId : ComparableSecurityGroupActionId -> SecurityGroupActionId
fromComparableSecurityGroupActionId comparableId =
    comparableId
        |> String.split "::"
        |> (\segments ->
                case segments of
                    "extant" :: uuid :: [] ->
                        ExtantGroup uuid

                    "new" :: name :: [] ->
                        NewGroup name

                    _ ->
                        ExtantGroup "unknown"
           )


type alias SecurityGroupAction =
    { pendingCreation : Bool
    , pendingDeletion : Bool
    , pendingServerChanges :
        { updates : Int
        , errors : List String
        }
    , pendingServerLinkage : Maybe ServerUuid
    , pendingSecurityGroupChanges :
        { updates : Int
        , errors : List String
        }
    , pendingRuleChanges :
        { creations : Int
        , deletions : Int
        , errors : List String
        }
    }


initSecurityGroupAction : SecurityGroupAction
initSecurityGroupAction =
    { pendingCreation = False
    , pendingDeletion = False
    , pendingServerChanges = { updates = 0, errors = [] }
    , pendingServerLinkage = Nothing
    , pendingSecurityGroupChanges = initPendingSecurityGroupChanges
    , pendingRuleChanges = initPendingRulesChanges
    }


initPendingSecurityGroupChanges : { updates : Int, errors : List String }
initPendingSecurityGroupChanges =
    { updates = 0
    , errors = []
    }


initPendingRulesChanges : { creations : Int, deletions : Int, errors : List String }
initPendingRulesChanges =
    { creations = 0
    , deletions = 0
    , errors = []
    }
