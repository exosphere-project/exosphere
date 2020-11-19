'use strict'

// get a reference to the div where we will show our UI
let container = document.getElementById('container')

var storedState = localStorage.getItem('exosphere-save');
var startingState = storedState ? JSON.parse(storedState) : null;

// Determine if running in Electron, per https://github.com/electron/electron/issues/2288#issuecomment-337858978
var userAgent = navigator.userAgent.toLowerCase();
var isElectron = (userAgent.indexOf(' electron/') > -1);

// Get a high-quality random seed to make a client UUID. We use 4 32-bit integers
// https://package.elm-lang.org/packages/TSFoster/elm-uuid/latest/UUID#generator
var typedArray = new Int32Array(4);
var randomSeeds = crypto.getRandomValues(typedArray);

// We need this to get the UTC offset?
var d = new Date();

if (isElectron) {
    var moduleToInit = Elm.ExosphereElectron.init;
} else {
    var moduleToInit = Elm.Exosphere.init;
}

// start the elm app in the container
// and keep a reference for communicating with the app
var app = moduleToInit({
    node: container,
    flags:
    {
        // Flags intended to be configured by cloud operators who offer Exosphere
        showDebugMsgs : false,
        cloudCorsProxyUrl: "https://try-dev.exosphere.app/proxy",
        cloudsWithUserAppProxy:
        [ ["iu.jetstream-cloud.org", "proxy-j7m-iu.exosphere.app"],
          ["tacc.jetstream-cloud.org", "proxy-j7m-tacc.exosphere.app"],
        ],
        urlPathPrefix : null,

        // Flags that Exosphere sets dynamically
        width: window.innerWidth,
        height: window.innerHeight,
        storedState: startingState,
        isElectron: isElectron,
        randomSeed0: randomSeeds[0],
        randomSeed1: randomSeeds[1],
        randomSeed2: randomSeeds[2],
        randomSeed3: randomSeeds[3],
        epoch : Date.now(),
        timeZone : d.getTimezoneOffset()
    }
});

app.ports.openInBrowser.subscribe(function (url) {
    if (isElectron) {
        // Open link in user's browser rather than Electron app
        const { shell } = require('electron')
        shell.openExternal(url)
    } else {
        window.open(url);
    }
});


app.ports.openNewWindow.subscribe(function (url) {
    if (isElectron) {
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
    } else {
        window.open(url);
    }
});

app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem('exosphere-save', JSON.stringify(state));
});

app.ports.instantiateClipboardJs.subscribe(function() {
  var clipboard = new ClipboardJS('.copy-button');
});
