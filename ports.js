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


app.ports.openNewWindow.subscribe(function (url) {
  // Open link in new Electron window, with 'nodeIntegration: false' so
  // Bootstrap will work.
  const electron = require('electron');
  const BrowserWindow = electron.remote.BrowserWindow;
  let newWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      nodeIntegration: false
    }
  });
  console.log('after constructor');
  newWindow.on('closed', () => {
    newWindow = null
  });

  // display the index.html file
  newWindow.loadURL(url);
});
