# Nomenclature Reference

Various projects and organizations who work with cloud computing use different words to refer to the same thing. This cross-reference shows the words used by Exosphere. In most cases, Exosphere has adopted the OpenStack term, except where a clearer/better term exists for the target audience (researchers and other non-IT professionals).

Note for users: an organization can offer a customized version of Exosphere with localized nomenclature (see localization section of README.md), so if you are using Exosphere somewhere other than [try.exosphere.app](https://try.exosphere.app), that interface may use different words. Ask your administrator for details.

Note for developers: some code in the Exosphere codebase (e.g. variable and function names) may use OpenStack terms instead of the "Exosphere new default term" listed below. Please see [issue 506](https://gitlab.com/exosphere/exosphere/-/issues/506) for ongoing work to resolve this.


| Description                                         | Exosphere new default term | OpenStack term(s)   | Jetstream term         | Amazon Web Services term   |
|-----------------------------------------------------|----------------------------|---------------------|------------------------|----------------------------|
| OpenStack deployment including its own Keystone     | cloud                      | cloud, cluster      | cloud                  | N/A                        |
| OpenStack that shares its Keystone with others      | region                     | region              | region                 | availability zone/region   |
| unit of tenancy                                     | project                    | project, (tenant)   | allocation             | account                    |
| maximum amount of resources your project can use    | resource limits            | quota, limits       | quota                  | AWS Service Quotas         |
| PKI public key used for SSH auth                    | SSH public key             | keypair             | SSH public key         | key pair                   |
| virtual computer                                    | instance?                  | server, (instance)  | instance               | instance                   |
| hardware configuration of a virtual computer        | size                       | size, flavor        | flavor                 | instance type and size     |
| console for troubleshooting a broken instance       | console                    | console             | console                | EC2 console                |
| buffer passed to cloud-init on first boot           | boot script                | user data           | boot script            | user data                  |
| static representation of hard drive contents        | image                      | image               | image                  | Amazon Machine Image (AMI) |
| storage block device                                | volume                     | volume              | volume                 | volume                     |
| non-floating IP address                             | internal IP address        | fixed IP address    | internal IP address    | private IPv4 address       |
| floating IP address                                 | public IP address          | floating IP address | public IP address      | Elastic IP Address         |
| text-based user interface served in a web browser   | terminal                   | N/A                 | web shell              | N/A                        |
| graphical desktop interface served in a web browser | graphical environment      | N/A                 | web desktop            | N/A                        |
