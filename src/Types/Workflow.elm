module Types.Workflow exposing
    ( CustomWorkflow
    , CustomWorkflowAuthToken
    , CustomWorkflowSource
    , CustomWorkflowSourceRepository(..)
    , CustomWorkflowTokenRDPP
    , ServerCustomWorkflowStatus(..)
    , SourceInput
    , SourceRepositoryPath
    , SourceRepositoryReference
    , WorkflowSourceResult(..)
    )

import Helpers.RemoteDataPlusPlus as RDPP
import Types.Error exposing (HttpErrorWithBody)
import Url



-- Note: Some of the types below are not used. Will be used when implementing:
-- https://gitlab.com/exosphere/exosphere/-/issues/564


type alias SourceRepositoryReference =
    String


type alias SourceRepositoryPath =
    String


type CustomWorkflowSourceRepository
    = GitRepository Url.Url (Maybe SourceRepositoryReference) -- e.g. "https://github.com/binder-examples/requirements" "main"
    | Doi String -- e.g. "10.5281/zenodo.3242074"


type alias CustomWorkflowAuthToken =
    String


type alias CustomWorkflowTokenRDPP =
    RDPP.RemoteDataPlusPlus HttpErrorWithBody CustomWorkflowAuthToken


type alias CustomWorkflowSource =
    { repository : CustomWorkflowSourceRepository
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
    { repository : String
    , reference : String
    , path : String
    }
