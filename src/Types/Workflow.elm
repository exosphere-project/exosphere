module Types.Workflow exposing
    ( CustomWorkflow
    , CustomWorkflowAuthToken
    , CustomWorkflowSource
    , CustomWorkflowSourceRepository(..)
    , CustomWorkflowTokenRDPP
    , ServerCustomWorkflowStatus(..)
    , SourceInput
    , SourcePathTypeInput(..)
    , SourceRepositoryPath
    , SourceRepositoryReference
    , WorkflowSourceResult(..)
    , sourcePathTypeInputOptions
    , sourcePathTypeInputToLabel
    , sourcePathTypeInputToOptions
    , sourcePathTypeInputToValue
    , stringToSourcePathType
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


type SourcePathTypeInput
    = InputFilePath
    | InputUrlPath


sourcePathTypeInputs =
    [ InputFilePath, InputUrlPath ]


sourcePathTypeInputToLabel : SourcePathTypeInput -> String
sourcePathTypeInputToLabel sourcePathType =
    case sourcePathType of
        InputFilePath ->
            "Path to a notebook file (optional)"

        InputUrlPath ->
            "URL to open (optional)"


sourcePathTypeInputToValue : SourcePathTypeInput -> String
sourcePathTypeInputToValue sourcePathType =
    case sourcePathType of
        InputFilePath ->
            "file"

        InputUrlPath ->
            "url"


sourcePathTypeInputToOptions : SourcePathTypeInput -> ( String, String )
sourcePathTypeInputToOptions sourcePathType =
    let
        sourcePathTypeString =
            sourcePathTypeInputToValue sourcePathType
    in
    case sourcePathType of
        InputFilePath ->
            ( sourcePathTypeString, "File" )

        InputUrlPath ->
            ( sourcePathTypeString, "URL" )


sourcePathTypeInputOptions : List ( String, String )
sourcePathTypeInputOptions =
    List.map sourcePathTypeInputToOptions sourcePathTypeInputs


stringToSourcePathType : String -> SourcePathTypeInput
stringToSourcePathType sourcePathTypeString =
    if (sourcePathTypeString |> String.toLower |> String.trim) == "url" then
        InputUrlPath

    else
        InputFilePath


type alias SourceInput =
    { providerPrefix : String
    , repository : String
    , reference : String
    , path : String
    , pathType : SourcePathTypeInput
    }
