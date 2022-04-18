# ADR 1: Using unpublished Elm dependencies

## Status

Accepted.

## Context

The [Elm Public Library](https://package.elm-lang.org/) is a catalog of all published packages which can be used as  dependencies in `elm.json`.

However, the published versions of packages might be unmaintained, have pending fixes, or require customisation in order to be used effectively. If this is the case, it is not straightforward how to proceed using unpublished packages & this can block progress.

## Decision

Use [elm-git-install](https://github.com/robinheghan/elm-git-install) to install an unpublished fork of the affected package.

Read more about [the options discussed](https://view.matrix.org/room/!XybqdsuDqzOURHcTIV:matrix.org/?anchor=%24qEMdkyNRU113uH-L3msmvaT5jVws0PBV3mIodXJwMd8&offset=210) in this decision & [view the originating MR](https://gitlab.com/exosphere/exosphere/-/merge_requests/646) for additional context.

## Consequences

The `elm-git-install` tool introduces an extra script step for fetching dependencies listed in `elm-git.json`. The cached package is then automatically added to `elm.json` under `source-directories`.

Dependencies added in this way ought to be periodically revisited to check their status: using published packages is preferred. If a package is updated to fix breaking issues, the published version can be used again. Conversely, substantial customisation of a package might make a case for publishing a new one to the Elm package registry.
