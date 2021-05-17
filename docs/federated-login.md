# Exosphere Federated Login Support

An OpenStack cloud (more specifically, OpenStack's Keystone identity service) can be configured to support federated user authentication using a third-party Identity Provider, like [CAS](https://apereo.github.io/cas/5.1.x/installation/OIDC-Authentication.html), [Shibboleth](https://wiki.shibboleth.net/confluence/display/DEV/Supported+Protocols), or [Keycloak](https://www.keycloak.org/docs/latest/securing_apps/). OpenStack supports two main technologies for this: SAML2.0 and OpenID Connect.

A deployment of Exosphere, as a client for OpenStack, can be customized to support use of federated login to an OpenStack cloud.  Currently, Exosphere only supports OpenID Connect, but we can extend this to other federated login methods that are supported by OpenStack, as needed by the community. If this is you, please [create a new issue](https://gitlab.com/exosphere/exosphere/-/issues/new) describing what you need! (SAML might even work already, it just hasn't been tested at this time.)

Also, if you have difficulty following this guide or need troubleshooting help, please create an issue or join our chat.

Important context:
- [Supporting user authentication for Jetstream 2](https://gitlab.com/exosphere/exosphere/-/issues/436), the issue that resulted in this feature getting built for Exosphere
- [Introduction to Keystone Federation](https://docs.openstack.org/keystone/latest/admin/federation/introduction.html)

## Benefits of Federated Login

- Users can log in with familiar institutional credentials; they do not need to manage an OpenStack-specific password
- Neither Exosphere nor OpenStack ever see or handle the user's institutional password
- Sophisticated authentication measures can be used: multi-factor, smart cards, whatever the identity provider supports.

For a small OpenStack deployment with just a few users, it may be easiest to just hand-create OpenStack accounts for each user, and have users manage their own OpenStack passwords. For institutions providing cloud infrastructure to many people, federated login is likely the most secure and convenient way to authenticate and authorize users.

### Explain OpenID Connect Like I'm Five

Imagine you're a guest at a fancy hotel, and the OpenStack cloud which you are trying to log into is a VIP lounge inside that hotel.

- You at the VIP lounge: "Let me in!"
- Bouncer: "Maybe, go get a ticket from the front desk."
- Front desk: "Hi, what's your name and room number?"
- You: "cmart, room 206."
- The front desk person looks you up in the reservation book, and confirms that you are Very Important. Then, they come up with a long random number (like 4187438906). They write this number next to your reservation, and also on a ticket for you.
- You bring this ticket back to the lounge and give it to the bouncer. The bouncer calls the front desk and reads off the number.
- Front desk, on the phone with the bouncer: "Oh yes, that ticket belongs to cmart, please let him in."
- The bouncer opens the door, you walk into the lounge and eat slices of fancy cheese.

This is an efficient way for the hotel to secure their lounge and authenticate guests, because the bouncer at the lounge doesn't need to keep track of all guests' hotel reservations, or whether they are very important. They only need to call the front desk and read the number on a guest's ticket, and the front desk will tell them whether to let the guest in.

In terms of art, the VIP lounge (a.k.a. OpenStack) is a _Service Provider_, and the front desk (a.k.a. Shibboleth, Google, Globus, etc.) is an _Identity Provider_.  Exosphere would play a relatively minor role in this analogy -- perhaps Exosphere is a concierge who shows you to the VIP lounge and the front desk.

## How to Configure Keystone

Before Exosphere can be configured to support federated login, [OpenStack Keystone must be configured first](https://docs.openstack.org/keystone/latest/admin/federation/configure_federation.html#setting-up-openid-connect
). Namely, Keystone and Apache must be configured as a Service Provider that talks to your external Identity Provider. That configuration is documented in the above link and is mostly beyond the scope of this document.

OpenStack documentation generally assumes that users will log in via [Horizon](https://docs.openstack.org/horizon/latest/), the default graphical interface for OpenStack. It's fine to set this up, even if only for testing purposes.

When you're ready to allow Keystone to handle federated login for Exosphere users,
you only need to [add another `trusted_dashboard`](https://docs.openstack.org/keystone/latest/admin/federation/configure_federation.html#add-a-trusted-dashboard-websso) in the `federation` section of Keystone config. This URL points to the OIDC Redirector app for your Exosphere deployment; go configure that below, then add its URL as a `trusted_dashboard` in your Keystone configuration.

## OIDC Redirector App

### About OIDC Redirector

Near the end of the OpenStack federated login flow, Keystone serves some JavaScript to the user's browser which causes the browser to POST some form data to a URL that we define in Exosphere and Keystone configuration. This POST data contains an unscoped Keystone token which will let the user access OpenStack! Exosphere needs this token so that it can help the user log into OpenStack projects, but herein lies a difficulty. Exosphere is a client-side JavaScript application which has no way to receive a POST request, so we need the help of a server-side application.

This server-side app is [**OIDC Redirector**](https://gitlab.com/exosphere/oidc-redirector/). It is a very small application written in Python using the [Flask](https://flask.palletsprojects.com/) web framework. OIDC Redirector does exactly the following:

- Receives the POST request from the user's browser
- Plucks the Keystone auth token out of the POST form data
- Redirects the user back to Exosphere, passing the auth token as a query parameter in the redirect URL.

Then Exosphere loads in the browser, parses the Keystone auth token out of the query string, and uses it to authenticate to OpenStack as the user. The federated login workflow is complete, and from here, Exosphere behaves exactly the same as if the user had logged in with raw OpenStack credentials.

### How to Set Up OIDC Redirector

[OIDC Redirector](https://gitlab.com/exosphere/oidc-redirector/) is a [Flask](https://flask.palletsprojects.com) application. You can run OIDC Redirector with any web server you please, but we provide a Dockerfile that makes it easy to run inside an Nginx + Flask container.

If your server environment is set up using docker-compose, just put something like this in your `docker-compose.yml`, alongside a clone of the oidc-redirector repo:

```
  oidc-redirector:
    build: './oidc-redirector'
    volumes:
      - './oidc-redirector:/app:ro'
    restart: 'always'
    environment:
        EXOSPHERE_URL: 'https://your.exosphere.domain'
```

These instructions assume that you are setting up OIDC Redirector behind a reverse proxy server, perhaps the same Nginx server that serves Exosphere and runs your [Cloud CORS Proxy](solving-cors-problem.md). (This is what the Exosphere project does for the Exosphere hosted apps, e.g. at <https://try.exosphere.app>.) Here is an example Nginx configuration:

```
upstream oidc-redirector {
    # This assumes that your oidc-redirector app is exposing port 80 and resolvable by Nginx as hostname "oidc-redirector", tweak as needed
    server oidc-redirector:80;
}

server {
    # Don't forget to set up TLS with your reverse proxy -- that is beyond the scope of this guide.
    listen 443 ssl;
    server_name your.exosphere.domain;

    location = /oidc-redirector {
        proxy_method POST;
        proxy_pass http://oidc-redirector/;
    }
}
```

## How to Configure Exosphere

Finally, Exosphere needs to know where to send users to complete the login process, and how to display the federated login button. You define these in the `config` JSON object in your `config.js`, as follows:

```
openIdConnectLoginConfig:
{ keystoneAuthUrl: "https://your.openstack.cloud:5000/v3",
  webssoKeystoneEndpoint: "/auth/OS-FEDERATION/websso/openid?origin=https://your.exosphere.domain/oidc-redirector",
  oidcLoginIcon: "path/to/your/desired/icon.png",
  oidcLoginButtonLabel : "Log in with your Medfield College NetID",
  oidcLoginButtonDescription : "Optional description to display below login button; this string can be left empty"
}
```

The `webssoKeystoneEndpoint` points to [this](https://docs.openstack.org/api-ref/identity/v3-ext/?expanded=#web-single-sign-on-authentication-new-in-version-1-2) Keystone endpoint. You likely won't need to change the path, but you do need to configure the `origin` query parameter. This origin must be the URL of the OIDC Redirector app for your Exosphere deployment, the same URL that you added as a `trusted_dashboard` in Keystone.

---

If everything is set up correctly, you should be able to browse to your Exosphere deployment and use the federated login method. Again, if you have difficulty getting this to work, the Exosphere developers may be able to help.