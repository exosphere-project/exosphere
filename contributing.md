## Contributor guidelines

- Be respectful
- Be constructive
- Be kind


## Submitting Code

Our CI pipeline runs:

- [elm-format](https://github.com/avh4/elm-format) (to ensure that contributions comply with the   
  [Elm Style Guide](https://elm-lang.org/docs/style-guide)
- [elm-analyse](https://stil4m.github.io/elm-analyse/)
  
 Please use `elm-format` and `elm-analyse` before you submit a merge request:
 
 ```bash
 npm install
 npm run elm:format
 npm run elm:analyse
 ```


## End-to-end browser tests

Our CI pipeline also runs integration tests with real browsers. For these tests to work you need:

1. Access to a Jetstream allocation
2. Valid TACC (Texas Advanced Computing Center) credentials
3. Set `taccusername` and `taccpass` environment variables in the GitLab CI/CD settings of your own fork of Exosphere

Ask the maintainers for testing credentials if you don't have a Jetstream allocation.

How to add TACC credentials as environment variables to your GitLab repository settings:

![Environment variables for end-to-end browser tests](docs/environment-variables-e2e-browser-tests.png)


## Architecture Decisions

We use lightweight architecture decision records. See: <https://adr.github.io/>

Our architecture decisions are documented in: [docs/adr/README.md](docs/adr/README.md)
