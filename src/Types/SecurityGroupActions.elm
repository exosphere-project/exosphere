module Types.SecurityGroupActions exposing (SecurityGroupAction, SecurityGroupActionId(..), initPendingRulesChanges, initPendingSecurityGroupChanges, initSecurityGroupAction, toComparableSecurityGroupActionId)

import OpenStack.Types exposing (SecurityGroupUuid)


type SecurityGroupActionId
    = ExtantGroup SecurityGroupUuid
    | NewGroup String


toComparableSecurityGroupActionId : SecurityGroupActionId -> String
toComparableSecurityGroupActionId actionId =
    case actionId of
        ExtantGroup uuid ->
            "extant-" ++ uuid

        NewGroup name ->
            "new-" ++ name


type alias SecurityGroupAction =
    { pendingCreation : Bool
    , pendingDeletion : Bool
    , pendingServerChanges :
        { updates : Int
        , errors : List String
        }
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
