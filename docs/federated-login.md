# Exosphere Federated Login Support

An OpenStack cloud (more specifically, OpenStack's Keystone identity service) can be configured to support federated user authentication using a third-party Identity Provider, like [CAS](https://apereo.github.io/cas/5.1.x/installation/OIDC-Authentication.html), [Shibboleth](https://wiki.shibboleth.net/confluence/display/DEV/Supported+Protocols), or [Keycloak](https://www.keycloak.org/docs/latest/securing_apps/). OpenStack supports two main technologies for this: SAML2.0 and OpenID Connect.

Currently, Exosphere only supports OpenID Connect, but we can extend this to other federated login methods that are supported by OpenStack, as needed by the community. If this is you, please [create a new issue](https://gitlab.com/exosphere/exosphere/-/issues/new) describing what you need!

Context:
- [Supporting user authentication for Jetstream 2](https://gitlab.com/exosphere/exosphere/-/issues/436), the issue that resulted in this feature getting built for Exosphere
- [Introduction to Keystone Federation](https://docs.openstack.org/keystone/latest/admin/federation/introduction.html)

## Benefits of Federated Login

- Users can log in with familiar institutional credentials; they do not need to manage an OpenStack-specific password

- Neither Exosphere nor OpenStack ever see or handle the user's institutional password
- Sophisticated authentication measures can be used: multi-factor, smart cards, whatever the identity provider supports.

For a small OpenStack deployment with just a few users, it may be easiest to just hand-create OpenStack accounts for each user, and have users manage their own OpenStack passwords. For institutions providing cloud infrastructure to many people, federated login is likely the most secure and convenient way to authenticate and authorize users.

## Explain Federated Auth Like I'm Five

If you have ever used your GitHub or Google account to log into a web-based service other than GitHub or Google, this works similarly. If not, then a rough allegory. Imagine that the OpenStack cloud which you are trying to log into is a VIP lounge inside a fancy hotel.

- You at the VIP lounge: "Let me in!"
- Bouncer: "Maybe, go check with the front desk."
- Front desk: "Hi, what's your name and room number?"
- You: "cmart, room 206."
- The front desk person looks you up in the reservation book, and confirms that you are Very Important. Then, they come up with a long random number (like 4187438906). They write this number next to your reservation, and also on a ticket for you, which you take back to the VIP lounge.
- You give this ticket to the bouncer, who then calls the front desk and reads off the number.
- Front desk, on the phone with the bouncer: "Oh yes, that ticket belongs to cmart. He is very important."
- The bouncer lets you in, and you eat slices of fancy cheese.

## Configure the application

Before Exosphere can be configured to support federated login, [OpenStack Keystone must be configured first](https://docs.openstack.org/keystone/latest/admin/federation/configure_federation.html#setting-up-openid-connect
).

Mostly beyond the scope of this document

OpenStack documentation generally assumes that users will log in via [Horizon](https://docs.openstack.org/horizon/latest/), the default graphical interface for OpenStack. Exosphere can be configured

Keystone is the OpenStack identity service, and most o

trusted dashboards

TODO Mike to advise

## TODO
- [ ] Parameterize Flask app and make a separate repo for it somewhere