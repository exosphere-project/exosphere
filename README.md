# Exosphere

User-friendly, extensible client for cloud computing. Currently targeting OpenStack.

## Collaborate

[Real-time  chat](https://c-mart.sandcats.io/shared/ak1ymBWynN1MZe0ot1yEBOh6RF6fZ9G2ZOo2xhnmVC5) (sign in with email or GitHub)

## Scaffold and Electron

To run this Electron app you will need to install Electron. You will also need elm, npm, and node.

Convenience commands to do this:

```bash
npm install
```

To compile and run the app:

```bash
npm run elm
npm run start
```

To watch for changes to `*.elm` files, auto-compile when they change, and hot-reloading of the app:

```bash
npm run watch
```

Based on the instructions found here:

<https://medium.com/@ezekeal/building-an-electron-app-with-elm-part-1-boilerplate-3416a730731f>

## Style Guide

### Imports

In the spirit of [PEP 8](https://www.python.org/dev/peps/pep-0008/), each file should import modules grouped into sections as follows:

1. Elm Standard libraries
2. Community libraries
3. Local app-specific libraries/imports

Unlike Python/PEP8 you will not be able to separate each section with a space because elm-format will remove the spaces. The spaces are not an Elm convention.

The imports in each section should be in alphabetical order.
