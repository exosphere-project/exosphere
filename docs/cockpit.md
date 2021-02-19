# Cockpit Deprecation and Manual Installation

With [this](https://gitlab.com/exosphere/exosphere/-/merge_requests/381) change, Exosphere is deprecating its integration with Cockpit. New servers are no longer automatically deployed with the Cockpit-based sever dashboard or terminal, and starting March 31, 2021, Exosphere will stop exposing Cockpit-based server interactions in the user interface.

Why? [Here](https://gitlab.com/exosphere/exosphere/-/issues/397) is some context, but in short, Apache Guacamole has proven to deliver a better remote access experience, while Cockpit has proven difficult to authenticate and secure for users.

The one thing Guacamole does not deliver is the web-based graphical server dashboard. If you rely on this feature of Cockpit, you can still set up Cockpit on your servers. It's not hard to do, because Cockpit is packaged for Ubuntu, CentOS, and other popular distros. On Ubuntu, run `sudo apt install cockpit`. On CentOS run `sudo yum install cockpit`. More detailed installation instructions are available [here](https://cockpit-project.org/running.html).

Note that you will need to log into Cockpit with the username `exouser` and the password exposed on the Server Details page in Exosphere.