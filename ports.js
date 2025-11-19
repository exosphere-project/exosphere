"use strict";

// get a reference to the div where we will show our UI
const container = document.getElementById("container");

var storedState = localStorage.getItem("exosphere-save");
var startingState = storedState ? JSON.parse(storedState) : null;

// Determine if running in Electron, per https://github.com/electron/electron/issues/2288#issuecomment-337858978
var userAgent = navigator.userAgent.toLowerCase();
var isElectron = userAgent.indexOf(" electron/") > -1;

// Get a high-quality random seed to make a client UUID. We use 4 32-bit integers
// https://package.elm-lang.org/packages/TSFoster/elm-uuid/latest/UUID#generator
var typedArray = new Int32Array(4);
var randomSeeds = crypto.getRandomValues(typedArray);

// Used on logout to ensure we don't save preferences
// during the logout sequence. On logout this gets set
// to disable updates, but then logout reloads the page
// which "re-enables" persistence by means of
// re-initializing the page.
let disablePersistence = false;

// We need this to get the UTC offset?
var d = new Date();

var electronDeprecationNotice = `
<html>
<body>
<p>This Electron-based desktop application has been deprecated.</p>
<p>Please start using Exosphere in your browser at https://try.exosphere.app. If you are a Jetstream2 user, please use https://jetstream2.exosphere.app.</p>
<p>Both of these sites support installation to your desktop or home screen; see https://gitlab.com/exosphere/exosphere/-/blob/master/docs/pwa-install.md for more information.</p>
</body>
</html>
`;

if (isElectron) {
  document.write(electronDeprecationNotice);
} else {
  var moduleToInit = Elm.Exosphere.init;
}

const themeDetector =
  "undefined" !== typeof window.matchMedia
    ? window.matchMedia("(prefers-color-scheme: dark)")
    : null;

const themePreference = (detector) => {
  if (!detector) {
    return null;
  }

  return detector.matches ? "dark" : "light";
};

var flags = {
  // Flags that Exosphere sets dynamically, not intended to be modified by deployer
  localeGuessingString: new Intl.NumberFormat(navigator.language).format(
    Math.PI * -1000000
  ),
  width: window.innerWidth,
  height: window.innerHeight,
  storedState: startingState,
  randomSeed0: randomSeeds[0],
  randomSeed1: randomSeeds[1],
  randomSeed2: randomSeeds[2],
  randomSeed3: randomSeeds[3],
  epoch: Date.now(),
  themePreference: themePreference(themeDetector),
  timeZone: d.getTimezoneOffset(),
};

const shim_config = window.__DEBUG__ ? { version: window.__VERSION__ } : {};

const merged_config = Object.assign({}, cloud_configs, config, shim_config);

// start the elm app in the container
// and keep a reference for communicating with the app
var app = moduleToInit({
  node: container,
  flags: JSON.stringify(Object.assign(flags, merged_config)),
});

app.ports.logout.subscribe(() => {
  disablePersistence = true;
  localStorage.clear();
  window.location.reload();
});

app.ports.openNewWindow.subscribe((url) => {
  window.open(url);
});

app.ports.setStorage.subscribe((state) => {
  if (disablePersistence) {
    return;
  }

  localStorage.setItem("exosphere-save", JSON.stringify(state));
});

app.ports.instantiateClipboardJs.subscribe(() => {
  const clipboard = new ClipboardJS(".copy-button");
});

app.ports.setFavicon.subscribe((url) => {
  // From https://stackoverflow.com/questions/260857/changing-website-favicon-dynamically
  var link = document.querySelector("link[rel~='icon']");
  if (!link) {
    link = document.createElement("link");
    link.rel = "icon";
    document.getElementsByTagName("head")[0].appendChild(link);
  }
  link.href = url;
});

// Note that this only does anything if an Exosphere environment is deployed with Matomo analytics. By default, it has no effect.
app.ports.pushUrlAndTitleToMatomo.subscribe((args) => {
  if (typeof _paq !== "undefined") {
    // From https://developer.matomo.org/guides/spa-tracking
    const currentUrl = args.newUrl;
    _paq.push(["setReferrerUrl", currentUrl]);
    _paq.push(["setCustomUrl", currentUrl]);
    _paq.push(["setDocumentTitle", args.pageTitle]);

    // remove all previously assigned custom variables, requires Matomo (formerly Piwik) 3.0.2
    _paq.push(["deleteCustomVariables", "page"]);
    _paq.push(["trackPageView"]);

    // make Matomo aware of newly added content
    const content = document.getElementById("content");
    _paq.push(["MediaAnalytics::scanForMedia", content]);
    _paq.push(["FormAnalytics::scanForForms", content]);
    _paq.push(["trackContentImpressionsWithinNode", content]);
  }
});

// Notify app of OS color scheme changes, if we can
themeDetector &&
  themeDetector.addEventListener("change", (detector) =>
    app.ports.changeThemePreference.send(themePreference(detector))
  );

// Fired when the browser has lost access to the network.
window.addEventListener("offline", (event) => {
  app.ports.updateNetworkConnectivity.send(false);
});
window.addEventListener("online", (event) => {
  app.ports.updateNetworkConnectivity.send(true);
});

const weblockResolvers = {};
// Request an exclusive web lock to synchronise resource access across tabs.
//  https://developer.mozilla.org/en-US/docs/Web/API/Web_Locks_API
app.ports.requestWebLock.subscribe((resource) => {
  // Request the lock.
  navigator.locks.request(
    resource,
    {
      // Don't wait if the lock is not available.
      ifAvailable: true,
      mode: "exclusive",
    },
    // Lock acquired.
    (lock) => {
      // Signal that we have the lock request result.
      app.ports.receiveWebLock.send([resource, !!lock]);

      // Was the lock acquired?
      if (!lock) {
        return;
      }

      // If the lock was granted, we hold the lock until the promise resolves.
      return new Promise((resolve) => {
        weblockResolvers[resource] = resolve;
      });
    }
  );
});

app.ports.releaseWebLock.subscribe((resource) => {
  // Resolve the promise to release the lock.
  if (weblockResolvers[resource]) {
    weblockResolvers[resource]();
    delete weblockResolvers[resource];
  }
});
