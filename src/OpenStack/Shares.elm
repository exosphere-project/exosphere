module OpenStack.Shares exposing (requestShares)

import Helpers.GetterSetters as GetterSetters
import Helpers.Time
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))


requestShares : Project -> Url -> Cmd SharedMsg
requestShares project url =
    let
        errorContext =
            ErrorContext
                "get a list of shares"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\shares ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveShares shares)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        -- `user_id` is only returned from v2.16 onwards
        [ ( "X-OpenStack-Manila-API-Version", "2.16" ) ]
        (url ++ "/shares/detail")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "shares" <| Decode.list shareDecoder)
        )


shareDecoder : Decode.Decoder OSTypes.Share
shareDecoder =
    Decode.succeed
        OSTypes.Share
        |> Pipeline.required "name" (Decode.maybe Decode.string)
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "status" (Decode.string |> Decode.map OSTypes.stringToShareStatus)
        |> Pipeline.required "size" Decode.int
        |> Pipeline.required "description" (Decode.maybe Decode.string)
        |> Pipeline.required "metadata" (Decode.dict Decode.string)
        |> Pipeline.required "created_at" (Decode.string |> Decode.andThen Helpers.Time.iso8601StringToPosixDecodeError)
        |> Pipeline.required "user_id" Decode.string
        |> Pipeline.required "is_public" (Decode.bool |> Decode.map OSTypes.boolToShareVisibility)
        |> Pipeline.required "share_proto" (Decode.string |> Decode.map OSTypes.stringToShareProtocol)
        |> Pipeline.required "share_type_name" Decode.string
