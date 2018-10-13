'use strict'

// get a reference to the div where we will show our UI
let container = document.getElementById('container')

// start the elm app in the container
// and keep a reference for communicating with the app
var app = Elm.Main.init({
    node: container,
    flags: 0
});

app.ports.openInBrowser.subscribe(function (url) {
    // Open link in user's browser rather than Electron app
    const { shell } = require('electron')
    shell.openExternal(url)
});
