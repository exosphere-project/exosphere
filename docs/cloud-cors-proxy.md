# Solving the CORS Problem

Connecting to cloud providers from Exosphere running in a browser is complicated by the [same-origin policy](https://en.wikipedia.org/wiki/Same-origin_policy). Exosphere makes HTTP requests to OpenStack clusters, and in a web browser this is considered a cross-origin credentialed request. Either of these two solutions will work:

1. Use a proxy server to handle Exosphere's communication to OpenStack providers
2. Connect Exosphere to OpenStack clouds whose Keystone is configured to allow cross-origin requests.
    - If you have administrative access to an OpenStack deployment then you can perform this configuration, see section below.

(todo are there more security implications that should be explained here?)

## Cloud CORS Proxy (CCP) Server

With a bit of configuration, [Nginx](https://nginx.org/en/) can handle all of Exosphere's requests to OpenStack providers.

### Configure Nginx

Here is an example Nginx configuration, the most relevant part is at the end where we pass the request on to the host and port that Exosphere specifies in the `exo-proxy-orig-host` and `exo-proxy-orig-port` headers.

```
server {
    listen 443 ssl;
    server_name dogfood.exosphere.app;

    ssl_certificate /etc/letsencrypt/live/dogfood.exosphere.app/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/dogfood.exosphere.app/privkey.pem;
    include /etc/letsencrypt/options-ssl-nginx.conf;
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem;

    location /exosphere {
        root /var/www/exo-webroot/;
    }

    location ~* ^/proxy/(.*) {
        resolver 127.0.0.11 [::1];
        set $backend https://$http_exo_proxy_orig_host:$http_exo_proxy_orig_port/$1$is_args$args;
        proxy_pass $backend;
    }
}
```

In order to make this work on your own server, you will need to do the following:
 - Change the server_name as appropriate for your own proxy server
 - Supply/generate your own TLS certificate
 - Store Exosphere as `/var/www/exo-webroot/exosphere`, and compile it (e.g. `elm make src/Exosphere.elm --output elm.js`), so that you are serving (e.g.) `https://dogfood.exosphere.app/exosphere/index.html`.

### Configure Exosphere to use Proxy Server
 
In (e.g.) `/var/www/exo-webroot/exosphere/ports.js`, look for the `flags` object passed to `Elm.Main.init`, and change the `proxyUrl` value from `null` to the URL of your proxy as follows:
 
```
var app = Elm.Main.init({
    node: container,
    flags:
    {
        width: window.innerWidth,
        height: window.innerHeight,
        storedState: startingState,
        proxyUrl: "https://dogfood.exosphere.app/proxy"
    }
});
```

---
 
Now you should be able to browse to `https://your-proxy-server/exosphere/` and use the app from there. If you click on "Help / About" in the upper-right corner, you should see that Exosphere is using your proxy server for communication to OpenStack. 

## Enable CORS in OpenStack

The OpenStack admin guide has a great page on how to enable CORS across OpenStack services. This guide was removed but is fortunately [still accessible via Wayback Machine](https://web.archive.org/web/20160305193201/http://docs.openstack.org/admin-guide-cloud/cross_project_cors.html).

At minimum, need the following in glance.conf, keystone.conf, cinder.conf(?), and neutron.conf:

```
[cors]
allowed_origin: *
```

The following in nova.conf:

```
[cors]
allowed_origin = *
allow_headers = Content-Type,Cache-Control,Content-Language,Expires,Last-Modified,Pragma,X-Custom-Header,OpenStack-API-Version,X-Auth-Token
```

Restart the appropriate services to pick up the new configuration. 