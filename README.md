# EXOSPHERE

A client for making things happen in the cloud. Currently targeting OpenStack.


## Scaffold and Electron

To run this Electron app you will need to install Electron. You will also need elm, npm, and node.

Follow this link to get all the steps to make this work:

<https://medium.com/@ezekeal/building-an-electron-app-with-elm-part-1-boilerplate-3416a730731f>

Then use `npm` to install Electron.

```
npm i -g elm electron-prebuilt
```

If you `elm-make` you can build the code for Exosphere then run it in Electron.

```
elm-make exosphere.elm --output elm.js
```

```
electron main.js
```
