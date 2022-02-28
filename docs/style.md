## UI, Layout, and Style

### Basics

- Exosphere uses [elm-ui](https://github.com/mdgriffith/elm-ui) for UI layout and styling. Where we can, we avoid defining HTML and CSS manually.
- Exosphere also consumes some [elm-ui-widgets](https://package.elm-lang.org/packages/Orasund/elm-ui-widgets/latest/)
- Exosphere also uses app-specific elm-ui "widgets", see `src/Widgets`.


### Design System

- Exosphere has a design system explorer (powered by [elm-ui-explorer](https://github.com/kalutheo/elm-ui-explorer)) showcasing Exosphere's widgets & their "story" variants.
- Launch the design system explorer (with live updates) using:

    ```bash
    npm run designs
    ```


### How to Add New Widgets

- Create a module for your widget (or update an existing module) in `src/Style/Widgets`.
- Add example usages of your widget in `src/DesignSystem/Explorer.elm`.
- Preview your widget's stories in the explorer to ensure they look & behave as intended.
- Where possible, try to use content representative of the context the widget will be used in.


### Style Guide (Legacy)

- There is an Exosphere "style guide" demonstrating the use of Exosphere's custom widgets.

- You can launch a live-updating Exosphere style guide by doing the following:
    + Run `npm run live-style-guide`
    + Browse to <http://127.0.0.1:8001>

- This guide will automatically refresh whenever you save changes to code in `src/Style`!

- You can also build a "static" style guide by running `npm run build-style-guide`. This will output styleguide.html.
