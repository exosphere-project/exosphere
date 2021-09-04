module Types.Workflow exposing
    ( CustomWorkflow
    , CustomWorkflowAuthToken
    , CustomWorkflowSource
    , CustomWorkflowTokenRDPP
    , ServerCustomWorkflowStatus(..)
    , SourceInput
    , SourceRepositoryIdentifier
    , SourceRepositoryPath
    , SourceRepositoryReference
    , WorkflowSourceResult(..)
    , sourceInputToWorkflowSource
    )

import Helpers.RemoteDataPlusPlus as RDPP
import Types.Error exposing (HttpErrorWithBody)


type alias SourceRepositoryIdentifier =
    String


type alias SourceRepositoryReference =
    String


type alias SourceRepositoryPath =
    String


type alias CustomWorkflowAuthToken =
    String


type alias CustomWorkflowTokenRDPP =
    RDPP.RemoteDataPlusPlus HttpErrorWithBody CustomWorkflowAuthToken


type alias CustomWorkflowSource =
    { repository : SourceRepositoryIdentifier
    , reference : Maybe SourceRepositoryReference
    , path : Maybe SourceRepositoryPath
    }


type alias CustomWorkflow =
    { source : CustomWorkflowSource
    , authToken : CustomWorkflowTokenRDPP
    }


type ServerCustomWorkflowStatus
    = NotLaunchedWithCustomWorkflow
    | LaunchedWithCustomWorkflow CustomWorkflow



-- Types used for input, before validated and turned into types above


type WorkflowSourceResult
    = Success CustomWorkflowSource
    | InvalidSource
    | EmptySource


type alias SourceInput =
    { repository : SourceRepositoryIdentifier
    , reference : SourceRepositoryReference
    , path : SourceRepositoryPath
    }


sourceInputToWorkflowSource : SourceInput -> WorkflowSourceResult
sourceInputToWorkflowSource sourceInput =
    let
        repository =
            if sourceInput.repository /= "" then
                Just sourceInput.repository

            else
                Nothing

        reference =
            if sourceInput.reference /= "" then
                Just sourceInput.reference

            else
                Nothing

        path =
            if sourceInput.path /= "" then
                Just sourceInput.path

            else
                Nothing

        eitherReferenceOrPathSpecified =
            case ( reference, path ) of
                ( Nothing, Nothing ) ->
                    False

                ( _, _ ) ->
                    True
    in
    case ( repository, eitherReferenceOrPathSpecified ) of
        ( Nothing, True ) ->
            InvalidSource

        ( Nothing, False ) ->
            EmptySource

        ( Just repo, _ ) ->
            Success
                { repository = repo
                , reference = reference
                , path = path
                }
