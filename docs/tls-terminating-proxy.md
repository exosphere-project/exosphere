## Problem Statement

Exosphere relies on a rich ecosystem of free, open-source software services to provide users with rich interactivity to their cloud servers (a.k.a. instances). A few important examples of these:

- [Apache Guacamole](http://guacamole.apache.org/), which allows users to access their servers with a remote terminal and graphical desktop environment, all from their web browser
- [Cockpit](https://cockpit-project.org) (Exosphere may deprecate this integration soon), which provides a graphical server management dashboard interface in the user's web browser
- [JupyterLab](https://jupyter.org/), which is not tightly integrated with Exosphere yet but is already used in the community of Exosphere users. (Several other data science tools and workbenches are also in this category.) 

What do these share in common? They are all web-based services. In the Exosphere ecosystem, they are all served from a user's cloud _server_, and accessed in the user's web browser (the _client_).

In order to serve a web-based service in a way that is reasonably secure and reliable, we must configure Transport Layer Security (TLS) on the _server_ end of the connection, and this is a challenge in the Exosphere ecosystem. To understand why, a high-level understanding of TLS is needed; skim or skip the following lesson if you don't need it.

### Brief TLS Lesson

Web browsers use [Transport Layer Security](https://en.wikipedia.org/wiki/Transport_Layer_Security) (TLS) to perform two critically important jobs: **confidentiality** and **server authentication**. Confidentiality assures you that the communication between browser and server is encrypted and cannot be intercepted or manipulated in transit. Server authentication assures you that the site/server you're connecting to at abc.com is actually the server which abc.com resolves to, and not an impostor ["man in the middle"](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) that's trying to pose as the server in order to steal sensitive information from the user.

To this end, a modern web browser has several restrictions regarding the kinds of connection it will make (and deem secure). Among these, it will only make a secure TLS connection to a site that presents a valid [server certificate](https://en.wikipedia.org/wiki/Public_key_certificate#TLS/SSL_server_certificate), and this certificate must be signed by one of a trusted set of [certificate authorities](https://en.wikipedia.org/wiki/Certificate_authority) (CAs). It is the CA's job to verify that the party requesting a certificate actually controls the server for the website that they wish to serve secure content for (e.g. abc.com). This is the basis of trust for the "server authentication" aspect of TLS[^tlsdisclaimer]. In other words, your browser will make secure (`https://`) connections to any server which presents a TLS certificate that was signed by a certificate authority that the browser trusts, and is valid for the hostname that the browser is connecting to. Any other type of `https://` connection will fail, by design, because it's not actually secure! In some cases the browser will warn the user that server's identity cannot be verified, and allow you to load the site after adding a security exception for it. But generally, such a connection should not be trusted with any sensitive information (like passwords).

Therefore, the cloud servers that are launched by Exosphere users must possess a valid server certificate signed by a browser-trusted CA (a "CA-signed cert" for short), in order to directly serve Guacamole, JupyterLab, and other services with sufficient security.

### End of TLS Lesson

**It appears infeasible to obtain a CA-signed cert for each server that a user launches, or at least infeasible on a large scale, and another solution is needed.**

To understand why, let's look at what's needed in order to obtain a CA-signed cert. This used to be a costly, manual process: you would pay a fee to a CA (like DigiCert or GoDaddy), then complete some manual challenge to prove that you own the hostname(s) that the certificate should be valid for. Then, the CA would send you the signed certificate. Fortunately, [Let's Encrypt](https://letsencrypt.org) has made this process free and automatic since late 2015. Let's Encrypt is great for many use cases, but it doesn't quite solve this problem. Let's Encrypt has a set of policy limitations which seem to make our use of it infeasible at scale (i.e. obtaining a certificate for each user-launched instance).

In brief: Let's Encrypt does not issue certificates for public IP addresses, only for DNS hostnames. When an Exosphere user launches an instance, that instance might only have an IP address. Obtaining a public hostname requires adding a host record to a domain, like mynewinstance.examplecloud.com. Exosphere generally won't have the ability to do this for users, though some OpenStack cloud operators (CyVerse and Jetstream) pre-create hostnames for their entire public IP address space, and services like [xip.io](http://xip.io/) also provide hostname coverage. Unfortunately, a hostname for each public IP address doesn't quite get us there, because Let's Encrypt also imposes [rate limits](https://letsencrypt.org/docs/rate-limits/) on certificate issuance per domain.

Under these rate limits, the only way to obtain a certificate for hundreds of hostnames per domain is to batch them, i.e. make each certificate valid for many hostnames (with each hostname corresponding to one public IP address or one cloud server). This would cause a security problem, because the certificates for different users' instances would share the same private key, which may allow one user to impersonate another user's instance to decrypt traffic and intercept sensitive information.  The only apparent workaround for these rate limits is to register many domains (one per public IP address), but this is costly (because registering a domain requires a yearly fee), thus it would not scale for an open-source software project.

## Solution: TLS-Terminating Reverse Proxy Server

Reverse proxies are widely used by those providing web-based services.

The proxy re-terminates TLS

we don't need a CA-signed cert on each instance, only one on the


Our use case is a little different because of the following:
- Proxy isn't hard-coded to communicate with just one or a few upstream servers 

Each OpenStack deployment that wishes to support the above activities can deploy a TLS-terminating (reverse) proxy server

Nginx

The idea is that there is a relatively small opportunity for man-in-the-middle between a TLS-terminating proxy server and instances on the same Neutron network.

(This has not been scrutinized super carefully from an infosec perspective.)

Nginx

Let's Encrypt now issues wildcard certificates

## Footnotes

[^tlsdisclaimer]: The Certificate Authority system is far from foolproof, and the possibility of [compromising a CA](https://en.wikipedia.org/wiki/Certificate_authority#CA_compromise) is the weak link in this arrangement, but that is beyond the scope of what Exosphere currently tries to accomplish.