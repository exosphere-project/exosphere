# Quick Start for New Contributors

Exosphere has a lighter-weight process for new contributors. If you're making your _first or second_ contribution to Exosphere, you only need this Quick Start guide. Otherwise, please see the [Core Contributor Onboarding section of contributing.md](../contributing.md#core-contributor-onboarding).

New contributors, please check with a project maintainer before attempting to solve an issue of weight 5 or higher.

This assumes familiarity with a git-based contribution workflow on platforms like GitHub. If you have never done that, or you are stuck for any other reason, ask for guidance [in our chat](https://matrix.to/#/#exosphere-dev:matrix.org). We're happy to help.

Exosphere is hosted on [gitlab.com](https://gitlab.com), a service that is similar to GitHub in many ways. One difference is that a code contribution on GitLab is called a _merge request (MR)_ instead of a _pull request (PR)_, but the concept and workflow is exactly the same.

- Create an account on [gitlab.com](https://gitlab.com), unless you have one already.
- Create your own fork of [exosphere/exosphere](https://gitlab.com/exosphere/exosphere).
- Clone your fork locally.
  - `git clone https://gitlab.com/your-gitlab-username/exosphere`
- Compile and run the app on your computer; see [Running Exosphere For Development Work](run-exosphere.md#for-development-work).
  - Optional but helpful step: [configure your editor](https://github.com/avh4/elm-format#editor-integration) to run `elm-format` whenever you save a file. Save `.elm` files often to automatically apply code formatting.
- Make your code changes, compile the app again, and confirm that your changes work.
  - Ask in chat if you need a set of credentials to test the app against a real OpenStack cloud.
- When you're satisfied with your changes, create a new branch, make a commit, and push the commit(s) to your origin on GitLab.
  - `git switch -c upside-down-support`
  - `git add *`
  - `git commit -m 'add a setting to display the entire app upside down'`
  - `git push -u origin upside-down-support`
- Browse to the URL in the output of your `git push` command to create a Merge Request.
  - Target this MR at the `master` branch of the upstream project (`exosphere/exosphere`).
  - Fill out the MR description template.
  - A maintainer will review your MR and respond if we need anything else from you.
- If you need to make more changes, continue committing and pushing them. Your merge request will update on each `git push`.