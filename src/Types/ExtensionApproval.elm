module Types.ExtensionApproval exposing
    ( ExtensionApproval
    , decoder
    , encode
    , forget
    , grant
    , isApproved
    , listDecoder
    )

{-| A persisted user decision that one specific VM instance is allowed to render the extension
UI it publishes (the `exoext` dynamic-UI mechanism). This is the mechanism's own standard, not
adapter-specific: the card adapter Exosphere ships is merely its first consumer, so nothing here
imports an adapter module.

Approval is matched by `instanceUuid` ONLY. The name is display-only: a different VM that
publishes an extension with the same name has a different UUID and therefore gets its own
opt-in prompt. `nameAtApproval`/`manifestEtagAtApproval` are captured for display and for a
future "the published UI changed since you approved it" check; they never affect matching.

The record decodes tolerantly (unknown fields are ignored, missing string fields default to
`""`) so that a later addition — e.g. a `disabled` flag — needs no storage version break.

-}

import Json.Decode as Decode
import Json.Encode as Encode


type alias ExtensionApproval =
    { cloudUrl : String
    , projectUuid : String
    , instanceUuid : String
    , nameAtApproval : String
    , approvedAt : String
    , manifestEtagAtApproval : String
    }


encode : ExtensionApproval -> Encode.Value
encode approval =
    Encode.object
        [ ( "cloudUrl", Encode.string approval.cloudUrl )
        , ( "projectUuid", Encode.string approval.projectUuid )
        , ( "instanceUuid", Encode.string approval.instanceUuid )
        , ( "nameAtApproval", Encode.string approval.nameAtApproval )
        , ( "approvedAt", Encode.string approval.approvedAt )
        , ( "manifestEtagAtApproval", Encode.string approval.manifestEtagAtApproval )
        ]


{-| Decode a single approval. `instanceUuid` is the one required field (matching depends on
it); every other string field is optional and defaults to `""`. Unknown fields are ignored,
which is what keeps the encoding forward-compatible.
-}
decoder : Decode.Decoder ExtensionApproval
decoder =
    Decode.map6 ExtensionApproval
        (optionalString "cloudUrl")
        (optionalString "projectUuid")
        (Decode.field "instanceUuid" Decode.string)
        (optionalString "nameAtApproval")
        (optionalString "approvedAt")
        (optionalString "manifestEtagAtApproval")


{-| Decode a stored list of approvals defensively: elements that fail to decode (e.g. a
missing `instanceUuid`) or carry an empty `instanceUuid` are dropped rather than failing the
whole list, so one corrupt record can never wipe out the rest of the store.
-}
listDecoder : Decode.Decoder (List ExtensionApproval)
listDecoder =
    Decode.list (Decode.maybe decoder)
        |> Decode.map (List.filterMap identity)
        |> Decode.map (List.filter (\approval -> approval.instanceUuid /= ""))


optionalString : String -> Decode.Decoder String
optionalString key =
    Decode.maybe (Decode.field key Decode.string)
        |> Decode.map (Maybe.withDefault "")


{-| Record an approval. A prior approval for the same `instanceUuid` is replaced, so
re-approving refreshes the captured name, etag, and timestamp.
-}
grant : ExtensionApproval -> List ExtensionApproval -> List ExtensionApproval
grant approval approvals =
    approval :: List.filter (\a -> a.instanceUuid /= approval.instanceUuid) approvals


{-| Forget the approval for an instance (removes the record; the card returns to its opt-in
affordance). Reversible by re-approving.
-}
forget : String -> List ExtensionApproval -> List ExtensionApproval
forget instanceUuid approvals =
    List.filter (\a -> a.instanceUuid /= instanceUuid) approvals


{-| Whether an instance currently has an approval. Matching is by `instanceUuid` only.
-}
isApproved : String -> List ExtensionApproval -> Bool
isApproved instanceUuid approvals =
    List.any (\a -> a.instanceUuid == instanceUuid) approvals
