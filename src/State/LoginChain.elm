module State.LoginChain exposing
    ( abandonCredentialFileChain
    , acceptsResponse
    , addNotice
    , dropCredentialFileTokens
    , findHeldToken
    , holdToken
    , nextGeneration
    , popNext
    , releaseHeldToken
    , shouldAbandon
    )

import List.Extra
import OpenStack.Types as OSTypes
import Types.LoginIntent exposing (HeldLoginToken)


nextGeneration : Int -> Int
nextGeneration generation =
    generation + 1


acceptsResponse : Int -> Int -> Bool
acceptsResponse currentGeneration responseGeneration =
    currentGeneration == responseGeneration


addNotice : String -> List String -> List String
addNotice projectName projectNames =
    if List.member projectName projectNames then
        projectNames

    else
        projectNames ++ [ projectName ]


popNext : List intent -> ( Maybe intent, List intent )
popNext intents =
    case intents of
        first :: remaining ->
            ( Just first, remaining )

        [] ->
            ( Nothing, [] )


{-| Abandon the credential-file chain: its pending and active intents, the tokens it holds, and any
response still in flight. A password login waiting on regions holds a token without an application
credential, so it keeps waiting.
-}
abandonCredentialFileChain :
    { model
        | pendingLoginIntents : List intent
        , activeLoginIntent : Maybe intent
        , heldLoginTokens : List HeldLoginToken
        , loginChainGeneration : Int
    }
    ->
        { model
            | pendingLoginIntents : List intent
            , activeLoginIntent : Maybe intent
            , heldLoginTokens : List HeldLoginToken
            , loginChainGeneration : Int
        }
abandonCredentialFileChain model =
    { model
        | pendingLoginIntents = []
        , activeLoginIntent = Nothing
        , heldLoginTokens = dropCredentialFileTokens model.heldLoginTokens
        , loginChainGeneration = nextGeneration model.loginChainGeneration
    }


shouldAbandon : Bool -> Bool -> Bool
shouldAbandon wasSelectingRegions willSelectRegions =
    wasSelectingRegions && not willSelectRegions


{-| A login waiting on a region question is known by its cloud and project, so holding a second
token for the same pair replaces the first while tokens for other projects wait alongside it.
-}
holdToken : HeldLoginToken -> List HeldLoginToken -> List HeldLoginToken
holdToken token heldTokens =
    token :: releaseHeldToken token.keystoneUrl token.authToken.project.uuid heldTokens


findHeldToken : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> List HeldLoginToken -> Maybe HeldLoginToken
findHeldToken keystoneUrl projectUuid heldTokens =
    List.Extra.find (isHeldFor keystoneUrl projectUuid) heldTokens


releaseHeldToken : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> List HeldLoginToken -> List HeldLoginToken
releaseHeldToken keystoneUrl projectUuid heldTokens =
    List.filter (not << isHeldFor keystoneUrl projectUuid) heldTokens


{-| Keep only the tokens a password login is waiting on. The credential-file chain holds tokens
with an application credential.
-}
dropCredentialFileTokens : List HeldLoginToken -> List HeldLoginToken
dropCredentialFileTokens heldTokens =
    List.filter (\token -> token.appCredential == Nothing) heldTokens


isHeldFor : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> HeldLoginToken -> Bool
isHeldFor keystoneUrl projectUuid token =
    token.keystoneUrl == keystoneUrl && token.authToken.project.uuid == projectUuid
