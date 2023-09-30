# Other Services

Below are examples of how to integrate services like user analytics and in-app user chat/support platforms with Exosphere.

(See [Configuration Options](./config-options.md#example-sentry-configuration) for integrating the Sentry application performance monitoring and error tracking software.)

## Matomo Analytics (formerly Piwik)

[Matomo Analytics](https://matomo.org/) is a privacy-protecting alternative to Google Analytics with a self-hosted option for 100% data ownership.

If you wish to use Matomo Analytics with Exosphere, then:

1. [Install Matomo on-premise](https://matomo.org/faq/on-premise/installing-matomo/) (or subscribe to the [cloud-hosted option](https://matomo.org/)) and set up a site for Exosphere in Matomo
2. [Get the JavaScript tracking code for your site](https://developer.matomo.org/guides/tracking-javascript-guide) (**Important**: only the code contained inside the `<script>` tag)
3. Add this JavaScript code to the bottom of your `config.js` file

Your `config.js` file should look something like this:

```javascript
"use strict";

var config = {
  showDebugMsgs: false,
  /* ... other config options ... */
};

/* Matomo tracking code */
var _paq = (window._paq = window._paq || []);
_paq.push(["trackPageView"]);
_paq.push(["enableLinkTracking"]);
(function () {
  var u = "//{$MATOMO_URL}/";
  _paq.push(["setTrackerUrl", u + "matomo.php"]);
  _paq.push(["setSiteId", "{$IDSITE}"]);
  var d = document,
    g = d.createElement("script"),
    s = d.getElementsByTagName("script")[0];
  g.type = "text/javascript";
  g.async = true;
  g.src = u + "matomo.js";
  s.parentNode.insertBefore(g, s);
})();
```

In your tracking code, `{$MATOMO_URL}` would be replaced by your Matomo URL and `{$IDSITE}` would be replaced by the id of the website you are tracking in Matomo.

## Chatwoot

The [Chatwoot](https://www.chatwoot.com/) customer engagement platform is an open-source, self-hosted alternative to Intercom and Zendesk. It provides a low-friction way for users to open chat conversations with support staff from within a web application. When enabled a user sees a chat icon on the bottom right of the Exosphere user interface. Pressing on this icon opens the chat interface.

If you wish to use Chatwoot with Exosphere, then:

1. [Install Chatwoot on-premise](https://www.chatwoot.com/docs/self-hosted) (or subscribe to the [cloud-hosted option](https://www.chatwoot.com/pricing))
2. [Add and configure a website channel](https://www.chatwoot.com/docs/product/channels/live-chat/create-website-channel/) for Exosphere in Chatwoot
3. Copy the JavaScript code for the Chatwoot widget from the last step above, or get it from [the website channel configuration page](https://www.chatwoot.com/docs/user-guide/setting-up-chatwootwidget/) (**Important**: only the code contained inside the `<script>` tag) 
4. Add this JavaScript code to the bottom of your `config.js` file

Your `config.js` file should look something like this:

```javascript
"use strict";

var config = {
  showDebugMsgs: false,
  /* ... other config options ... */
};

/* Chatwoot widget code */
(function(d,t) {
  var BASE_URL="https://{$CHATWOOT_URL}";
  var g=d.createElement(t),s=d.getElementsByTagName(t)[0];
  g.src=BASE_URL+"/packs/js/sdk.js";
  g.defer = true;
  g.async = true;
  s.parentNode.insertBefore(g,s);
  g.onload=function(){
    window.chatwootSDK.run({
      websiteToken: '{$CHATWOOT_WEBSITE_TOKEN}}',
      baseUrl: BASE_URL
    })
  }
})(document,"script");
```

In your widget code, `{$CHATWOOT_URL}` would be replaced by your Chatwoot URL and `{$CHATWOOT_WEBSITE_TOKEN}` would be replaced by the token of the website with which you want to integrate Chatwoot.
