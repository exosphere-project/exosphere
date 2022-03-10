# Configuring Flavor Groups

Exosphere enables cloud administrators to group flavors. Why is this useful?

Imagine you have 'general-purpose' flavors `m1.small` and `m1.large`, and GPU-endowed flavors `g1.small` and `g1.large`.

You probably want to show users a flavor selection UI with the general-purpose flavors grouped together at the top, followed by the GPU-endowed flavors. Maybe it would look like this:

> #### General-purpose
> - m1.small
> - m1.large
> #### GPU 
> - g1.small
> - g1.large

Flavor groups allow you to build this UI in Exosphere.

The flavor groups are defined in `config.js`, within each cloud of the `clouds` array, as a `flavorGroups` array. (See the "Runtime configuration options" section of README.md for an overview of how this works.) Scroll to the bottom for an example of this JSON.

## flavorGroups

Each array element in `flavorGroups` is an object with the following members:

- `matchOn` (string) is a regular expression that matches names of flavors belonging to the group.
- `title` (string) is something like "General-purpose" or "GPU", used as a heading for the group.
- `description` (null or string) is optional help text that appears in a toggle tip next to the heading.

A few hints:
- The order in which you specify flavor groups is the same order in which they will appear in Exosphere.
- At this time, it's possible to define flavor groups such that the same flavor appears in multiple groups.
- It's possible to define an array of flavor groups such that one or more of your flavors do not belong to any groups. At this time, those flavors will not be displayed at all.
- If you do not define any flavor groups, then Exosphere will just display all flavors in a flat list.

## Example instanceTypes JSON


```javascript
var config = {
  ...
  "clouds":[
    {
      "keystoneHostname":"iu.jetstream-cloud.org",    
      ...  
      "flavorGroups":[
        {
          "matchOn":"m1\..*",
          "title":"General-purpose",
          "description":null
        },
        {
          "matchOn":"g1\..*",
          "title":"GPU",
          "description":"These have a graphics processing unit."
        }        
      ]
    }
  ]
}
```