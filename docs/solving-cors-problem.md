# Solving the CORS Problem

Connecting to cloud providers from Exosphere running in a browser is complicated by the [same-origin policy](https://en.wikipedia.org/wiki/Same-origin_policy). Exosphere makes HTTP requests to OpenStack APIs, and in a web browser this is considered a cross-origin credentialed request. Either of these two solutions will allow these requests to complete:

1. Use a proxy server to handle Exosphere's communication to OpenStack providers
2. Connect Exosphere to OpenStack clouds whose Keystone is configured to allow cross-origin requests.
   - If you have administrative access to an OpenStack deployment then you can perform this configuration, see section below.


## 1. Cloud CORS Proxy (CCP) Server

With a bit of configuration, [Nginx](https://nginx.org/en/) can handle all of Exosphere's requests to OpenStack providers. This is a reliable solution that works with all browsers.

Nginx must be exposed on a network that users can connect to with their web browser (e.g. on TCP port 443), either on the internet or on an institutional network that users have access to (perhaps with a VPN).

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
 - Change the server_name to the hostname of your proxy server
 - Supply/generate your own TLS certificate that is signed by a root CA that the user's browser trusts. Let's Encrypt gives these out for free for internet-exposed servers. 
 - Store the Exosphere repository as `/var/www/exo-webroot/exosphere`, and compile it (e.g. `elm make src/Exosphere.elm --output elm.js`), so that you are serving (e.g.) `https://dogfood.exosphere.app/exosphere/index.html`.

### Configure Exosphere to use Proxy Server
 
In (e.g.) `/var/www/exo-webroot/exosphere/config.js`, look for the `cloudCorsProxyUrl` key, and change the value to the URL of your proxy as follows:
 
```
var config = {
  ...
  cloudCorsProxyUrl: "https://dogfood.exosphere.app/proxy",
  ...
```

---
 
Now you should be able to browse to `https://your-proxy-server/exosphere/` and use the app from there. If you click on "Help / About" in the upper-right corner, you should see that Exosphere is using your proxy server for communication to OpenStack. 

## 2. Enable CORS in OpenStack

This method may be less reliable for users in mixed network environments where outbound TCP ports (e.g. 5000, 8774, 9292, 9696) are blocked by edge firewalls. It will likely work if the user's browser is on the same institutional network as the OpenStack APIs (or perhaps connected with a VPN).

The OpenStack admin guide has a great page on how to enable CORS across OpenStack services. This guide was removed but is fortunately [still accessible via Wayback Machine](https://web.archive.org/web/20160305193201/http://docs.openstack.org/admin-guide-cloud/cross_project_cors.html).

At minimum, need the following in glance.conf, keystone.conf, cinder.conf, and neutron.conf:

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

If you wish for users to connect directly to OpenStack (not using a Cloud CORS Proxy server), modify `config.js` wherever you are serving Exosphere, and set the `cloudCorsProxyUrl` key to null:

```
var config = {
  ...
  cloudCorsProxyUrl: null,
  ...
```
