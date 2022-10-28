# Tour of Exosphere Codebase

This document orients new contributors to the general structure of Exosphere's codebase. It is a continuous work in progress, and is not intended to be exhaustive -- the code itself is the source of truth.

If you are new to Elm as well as Exosphere, this guide pairs well with the official [Elm Guide](https://guide.elm-lang.org), particularly the [Elm Architecture](https://guide.elm-lang.org/architecture) pages.

## Nomenclature

- Herein, "the application" or simply "the app" refers to the Exosphere Elm application that compiles to JavaScript and runs in the user's web browser.

## Main function

Exosphere's Elm source code lives in the `src/` directory. The main file is `Exosphere.elm`. When you run the Elm compiler, you're pointing it there.

Go look at `Exosphere.elm`, you'll see it has just one `main` function. This function calls `Browser.application` to start the app. It passes several arguments. The most important ones are:

- The `init` function builds the initial state of the app, i.e., it initializes the Model that is persisted in memory while the app is running. If there is an existing saved user session (in the browser's local storage), `init` will decode that stored state and 'hydrate' the Model from it. `init` also fires initial `Cmd`s (pronounced "commands"). (These make API calls (generally to an OpenStack back-end), and call out from Elm to JavaScript as needed.) You can see the `init` function in `src/State/Init.elm`.
- The `view` function decides what to show to the user on the screen. It takes as input the application state (i.e. Model, specifically here `OuterModel`). It outputs HTML and CSS (along with a page title) that is rendered by the user's web browser, along with a page title. You can see the top-level `view` function in `src/View/View.elm`, though it imports many other modules that we'll visit later in the tour.
- The `update` function is responsible for advancing the state of the app forward, in response to external events (like button clicks and results of API calls). It receives a `Msg` (pronounced "message") from the Elm runtime, and the current state (Model) of the app. Then, it computes the new state of the app. It returns a new Model (representing the new state) and a new `Cmd`. That `Cmd` tells the Elm runtime to do things like make HTTP requests to external services, and call JavaScript outside the Elm app.

This uses the same basic architecture described in the Elm guide, but with more modules and helper code.


## View, Pages, and Style Code

`src/View/View.elm` contains the top-level `view` function that's passed to the main function. `view` handles the possibility of an invalid application state, but if it's valid, it calls a stack of functions that:

- Provide a page title (`viewValid`)
- Set up a base [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui) layout (`view_`)
- Draw a page header and a main content container (`appView`)
- Render a non-project-specific (`nonProjectViews`) or project-specific (`projectContentView`) view, depending on the current view state.

From here, we call out from the top-level view code to individual pages. You'll see that `View.elm` imports many modules starting with `Page.`, and calls the view function for each of these pages.

Modules in `src/Page` usually correspond to a specific page in the application. Most pages have their own Elm architecture -- each page has its own `Model` and `Msg` types, and `init`, `update`, and `view` functions. Each page's architecture is nested inside of the overall app's architecture.

Further down the call stack, we have `src/Style` code. This includes `src/Style/Widgets`, self-contained UI widgets that can each be re-used on many pages. These widgets are generally stateless -- they don't have their own Elm architecture, just a view function that takes parameters and renders the widget. Widgets are also generally independent from the Exosphere codebase. Someday, they could live in a separate Elm package.

There are also helper functions in both `src/View` and `src/Style`. The distinction is that `View` helpers are specific to Exosphere's codebase, while `Style` helpers could be independent. You may find exceptions, but that is the intention going forward.