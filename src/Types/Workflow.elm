module Types.Workflow exposing
    ( CustomWorkflow
    , CustomWorkflowAuthToken
    , CustomWorkflowSource
    , CustomWorkflowSourceRepository(..)
    , CustomWorkflowTokenRDPP
    , ServerCustomWorkflowStatus(..)
    , SourceInput
    , SourcePathInput(..)
    , SourceProvider
    , SourceRepositoryPath(..)
    , SourceRepositoryReference
    , WorkflowSourceResult(..)
    , defaultProvider
    , providers
    )

import Helpers.RemoteDataPlusPlus as RDPP
import Types.Error exposing (HttpErrorWithBody)
import Url



-- Note: Some of the types below are not used. Will be used when implementing:
-- https://gitlab.com/exosphere/exosphere/-/issues/564


type alias SourceRepositoryReference =
    String


type
    SourceRepositoryPath
    -- This is an optional field. It captures the URL or file path which repo2docker uses as the default notebook/URL to
    -- show when launching the container.
    = FilePath String
    | UrlPath String


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


type alias SourceProvider =
    { text : String
    , tagText : String
    , refPropDisabled : Bool
    }


type SourcePathInput
    = InputFilePath
    | InputUrlPath


defaultProvider =
    SourceProvider "GitHub repository name or URL" "Git ref (branch, tag, or commit)" False


providers =
    [ ( "GitHub", defaultProvider )
    , ( "Gist", SourceProvider "Gist ID (username/gistId) or URL" "Git commit SHA" False )
    , ( "Git repository", SourceProvider "Arbitrary git repository URL (http://git.example.com/repo)" "Git ref (branch, tag, or commit)" False )
    , ( "GitLab.com", SourceProvider "GitLab.com repository or URL" "Git ref (branch, tag, or commit)" False )
    , ( "Zenodo DOI", SourceProvider "Zenodo DOI (10.5281/zenodo.3242074)" "Git ref (branch, tag, or commit)" True )
    , ( "Figshare DOI", SourceProvider "Figshare DOI (10.6084/m9.figshare.9782777.v1)" "Git ref (branch, tag, or commit)" True )
    , ( "Hydroshare resource", SourceProvider "Hydroshare resource id or URL" "Git ref (branch, tag, or commit)" True )
    , ( "Dataverse DOI", SourceProvider "Dataverse DOI (10.7910/DVN/TJCLKP)" "Git ref (branch, tag, or commit)" True )
    ]


type alias SourceInput =
    { providerPrefix : String
    , repository : String
    , reference : String
    , path : String
    , pathType : SourcePathInput
    }
