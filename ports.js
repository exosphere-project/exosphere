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

var flags = {
    // Flags that Exosphere sets dynamically, not intended to be modified by deployer
    width: window.innerWidth,
    height: window.innerHeight,
    storedState: startingState,
    randomSeed0: randomSeeds[0],
    randomSeed1: randomSeeds[1],
    randomSeed2: randomSeeds[2],
    randomSeed3: randomSeeds[3],
    epoch : Date.now(),
    timeZone : d.getTimezoneOffset()
}


// start the elm app in the container
// and keep a reference for communicating with the app
var app = moduleToInit({
    node: container,
    flags: Object.assign(flags, config)
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

app.ports.setFavicon.subscribe(function (url) {
    // From https://stackoverflow.com/questions/260857/changing-website-favicon-dynamically
    var link = document.querySelector("link[rel~='icon']");
    if (!link) {
        link = document.createElement('link');
        link.rel = 'icon';
        document.getElementsByTagName('head')[0].appendChild(link);
    }
    link.href = url;
});

// Note that this only does anything if an Exosphere environment is deployed with Matomo analytics. By default, it has no effect.
app.ports.pushUrlAndTitleToMatomo.subscribe(function(args) {
    if (typeof _paq !== 'undefined') {
        // From https://developer.matomo.org/guides/spa-tracking
        var currentUrl = args.newUrl;
        _paq.push(['setReferrerUrl', currentUrl]);
        _paq.push(['setCustomUrl', currentUrl]);
        _paq.push(['setDocumentTitle', args.pageTitle]);

        // remove all previously assigned custom variables, requires Matomo (formerly Piwik) 3.0.2
        _paq.push(['deleteCustomVariables', 'page']);
        _paq.push(['trackPageView']);

        // make Matomo aware of newly added content
        var content = document.getElementById('content');
        _paq.push(['MediaAnalytics::scanForMedia', content]);
        _paq.push(['FormAnalytics::scanForForms', content]);
        _paq.push(['trackContentImpressionsWithinNode', content]);
    }
});