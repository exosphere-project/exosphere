module Types.SecurityGroupActions exposing (SecurityGroupAction, initSecurityGroupAction)


type alias SecurityGroupAction =
    { pendingDeletion : Bool
    , pendingServerChanges :
        { updates : Int
        , errors : List String
        }
    }


initSecurityGroupAction : SecurityGroupAction
initSecurityGroupAction =
    { pendingDeletion = False
    , pendingServerChanges = { updates = 0, errors = [] }
    }
