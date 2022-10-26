# Message for Desktop Environment Users

When a user creates an instance, they have the option of enabling a desktop environment on that instance. This results in a few things:

- On first boot, the instance installs packages that provide a graphical desktop environment (unless they are already installed).
- Exosphere sets the instance's default systemd target to `graphical`, which enables the desktop environment.
- Exosphere enables Apache Guacamole to deliver desktop session to the user's web browser.

A few things could go wrong with this, namely:
- Exosphere only knows how to set all this up on modern Ubuntu- (20+) and CentOS-based (8+) operating systems. So, if the user creates an instance from an image with a different operating system, desktop environment setup may fail.
- If the image doesn't have a graphical desktop environment pre-installed, instance setup may take a long time (30 minutes or more), because many hundreds of packages must be installed on first boot.
- The image that the user selects may be broken in various ways that prevent the desktop from working.

To help the user avoid creating a surprisingly broken instance, Exosphere displays a warning message when someone enables the desktop environment during instance creation. This message reads (approximately, subject to your localization settings):

> Graphical desktop works for images based on Ubuntu (20.04 or newer), Rocky Linux, or AlmaLinux. If you selected a different operating system, it may not work. Also, if selected image does not have a desktop environment pre-installed, instance may take a long time to deploy.

As a cloud operator, you might have mitigated these concerns, e.g., by offering only public images which have supported operating systems and a desktop environment pre-installed. Or, you may have different guidance for the user if they enable a desktop environment. So, Exosphere provides you with the ability to override this generic warning message. You can override the message at two levels:

- **At the image-specific level**, by setting a value for the `exoDesktopMessage` image metadata property. An empty string value will display no message at all.
- **At the cloud level**, by setting the value of `desktopMessage` in cloud-specific configuration. See [Example Cloud Configuration](../README.md#example-cloud-configuration) for more details.

A message set at the image-specific level overrides a message set at the cloud level.