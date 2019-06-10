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

