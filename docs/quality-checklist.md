# Merge Request Quality Checklist

Reviewers, please ensure every MR passes this checklist before approving, including MRs from new contributors. Consult co-reviewers when making the occasional exception.

## Administrative

- MR description is fully populated.
- MR effectively fixes all issues that it claims to fix.
  - If not, change the `fixes #123` text in the description (e.g. `fixes part of #123`)
- Follow-up issues are created for any new issues that the MR causes or uncovers.
  - If the MR introduces any technical debt, these issues are assigned to MR author, unless they are a first- or second-time contributor.

## Quality and Technical Debt

Relax the criteria in this section if this is a contributor's first or second MR, _and_ any technical or UI debt introduced is modest (the fix would fit on about 1 screen of code), _and_ you create a follow-up issue to track it.

- MR does not decrease the overall consistency or polish of Exosphere's UI.
- MR does not decrease Exosphere's overall code quality.
- MR does not use hard-coded representations of any [localized strings](nomenclature-reference.md) in the UI.
- If the MR adds/changes padding and spacing, numbers from `spacer` must be used, and the guidelines at "Space" section of design system should be followed.

## Functional

- MR does not break existing functionality or behavior that users are likely to care about.
- If the MR adds/changes/removes app flags (in `src/Types/Flags.elm`), then the following are updated accordingly:
  + `types.d.ts`
  + `config.js`
  + all files in `environment-configs/` **(else you may break production sites!)**
  + Documented options in [config-options.md](config-options.md)
- If the MR adds [localized strings](nomenclature-reference.md) (in `src/Types/Defaults.elm`) then the following are updated accordingly:
  + `types.d.ts`
  + all files in `environment-configs/` which have `localization` specified **(else you may break production sites!)**
  + Documented options for `Example Localization JSON object` section in [config-options.md](config-options.md)
  + `exosphereLocalizedStrings` in `review/src/NoHardcodedLocalizedStrings.elm`
- If the MR adds/changes/removes any popovers, ensure that their IDs are unique.

## Documentation

- If MR significantly changes organization structure of codebase (e.g. modules and directories), `docs/code-tour.md` is updated appropriately.
- If the MR adds/changes/removes UI elements in `src/Style/Widgets/`, then `src/DesignSystem/Explorer.elm` shows example usage of that widget.