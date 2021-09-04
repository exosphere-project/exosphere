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
    )

import Helpers.RemoteDataPlusPlus as RDPP
import Types.Error exposing (HttpErrorWithBody)



-- Note: Some of the types below are not used. Will be used when implementing:
-- https://gitlab.com/exosphere/exosphere/-/issues/564


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


type alias SourceInput =
    { repository : SourceRepositoryIdentifier
    , reference : SourceRepositoryReference
    , path : SourceRepositoryPath
    }
