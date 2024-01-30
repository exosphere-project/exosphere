module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoElementFontSize
import NoExposingEverything
import NoHardcodedLocalizedStrings
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoUnused.Dependencies
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


inPaths : List String -> String -> Bool
inPaths paths path =
    List.any
        (\pfx -> String.startsWith pfx path)
        paths


notInPaths : List String -> String -> Bool
notInPaths paths path =
    not <| inPaths paths path


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoDeprecated.rule NoDeprecated.defaults
    , NoElementFontSize.rule
        |> Rule.filterErrorsForFiles ((/=) "src/Style/Widgets/Text.elm")
    , NoConfusingPrefixOperator.rule
    , NoHardcodedLocalizedStrings.rule NoHardcodedLocalizedStrings.exosphereLocalizedStrings
        |> Rule.filterErrorsForFiles
            (notInPaths
                [ "tests/"
                , "src/Route.elm"
                , "src/State/Init.elm"
                , "src/State/Error.elm"
                , "src/Helpers/Helpers.elm"
                , "src/Rest/"
                , "src/Types/"
                , "src/DesignSystem/"
                , "src/LocalStorage/"
                , "src/OpenStack/"
                ]
            )
    , NoMissingTypeExpose.rule
        |> Rule.filterErrorsForFiles
            (notInPaths
                [ "src/DesignSystem/Explorer.elm"
                , "src/Exosphere.elm"
                ]
            )
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Variables.rule
    ]
