module Types.SecurityGroupActions exposing (SecurityGroupAction, initSecurityGroupAction)


type alias SecurityGroupAction =
    { pendingCreation : Bool
    , pendingDeletion : Bool
    , pendingServerChanges :
        { updates : Int
        , errors : List String
        }
    }


initSecurityGroupAction : SecurityGroupAction
initSecurityGroupAction =
    { pendingCreation = False
    , pendingDeletion = False
    , pendingServerChanges = { updates = 0, errors = [] }
    }
