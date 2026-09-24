# Object Storage

Object Storage lets you browse and manage your cloud's [Swift](https://docs.openstack.org/swift/latest/) object storage from Exosphere: containers, objects, pseudo-folders, uploads and downloads, bulk actions, and sharing.

The Object Storage tile and its create action appear only when **Experimental features** is enabled in Settings and your project's service catalog offers a Swift `object-store` endpoint (see [When object storage is not available](#when-object-storage-is-not-available)).

## What you can do

A per-project **Object Storage** tile appears on your Project page when Experimental features is enabled and the service is available. From there you can:

- **Containers** — list every container with its object count and size, create a container, and delete one (a non-empty container is emptied first, behind a confirmation).
- **Objects and folders** — browse the objects and pseudo-folders inside a container, navigate folders with breadcrumbs, and page through large containers.
- **Upload and download** — add files from your computer, and download any object.
- **Copy and move** — duplicate an object to another container or path, or move it.
- **New folder** — create an empty folder at the current level.
- **Manage access** — make a container public or private, share it with specific people or a whole project, and copy public links.
- **Connect an S3 client** — if your cloud also offers an S3-compatible API, copy ready-made connection settings for tools like rclone or Cyberduck.

## The container list

The Object Storage page lists your containers. Each row links to the container's contents and shows its object count and size, with quick actions to copy the name, jump to its sharing settings, or delete it. A search box filters the list by name.

To **create a container**, type a name and press **Create**. Container names cannot contain a slash and have a length limit; Exosphere flags an invalid or duplicate name as you type. **Names can't be changed after creation**, so pick one you are happy with.

To **delete several containers at once**, select them and use the bulk delete action. Deleting a non-empty container permanently deletes every object inside it first, then the container, behind a confirmation that spells out how many objects are affected.

> **Large objects.** Emptying a container deletes ordinary objects. It does not understand large-object (SLO/DLO) manifests, so deleting one can leave its segments behind as orphaned data that still counts against your quota. Clean up large objects with the OpenStack/Swift CLI or [rclone](https://rclone.org/) instead.

## Inside a container

Opening a container shows its contents, with a header that carries the container name, an **info (?)** button, and an **Actions** menu.

- The **info button** shows details that most users rarely need: the storage policy, a summary of who has access, and — when your cloud offers S3 — the matching S3 bucket name.
- The **Actions menu** lets you copy the container's URL, make it public or private, and delete it. When the container is public, it also offers a one-click copy of its public link.
- An **info strip** below the header shows when the container was created, its object count and size, and whether it is public or private, alongside a copyable link to the container.

### Folders and breadcrumbs

Swift has no real directories. Exosphere shows the familiar folder view by treating the `/` in object names as a separator and listing one level at a time. Folder rows open into that level; a **breadcrumb trail** and an **up-one-level** button let you climb back out. Both appear only while you are inside a folder — at the container root they would just repeat the container name. Underneath, the objects are still flat keys like `reports/2026/q1.csv`.

To make a folder, choose **New folder** and name it. Exosphere writes a harmless zero-byte placeholder so the empty folder shows up; you never need to remove it, and deleting the container cleans it up like any other object.

### Object actions

Each object row shows its size and when it was last modified, plus actions to:

- **copy the object's name**,
- **copy its public link** (only when the container is public),
- **download** it,
- **copy** or **move** it to another container or path — this happens on the server, so the bytes never pass through your browser (a move copies, then deletes the original once the copy succeeds), and
- **delete** it.

Copy and move are single-object actions; folders cannot be copied. You can also select several objects and delete them in one action.

Very large containers load a page at a time (up to 10,000 rows per page). When there are more, a **Load more** button fetches the next page.

### Uploading files

Choose **Upload files** and pick one or more files. Uploads run through a queue so you can add several at once; each shows a status of Queued, Uploading, Uploaded, or Failed. Once some are finished, **Clear finished** tidies them away while any still in flight keep going. There is no percentage progress bar.

Exosphere reads each file fully into the browser and sends it in one request, so there is a hard limit of **100 MiB per file**. Larger files are rejected up front with a clear message. For bigger uploads — or many-gigabyte objects — use the OpenStack/Swift CLI or [rclone](https://rclone.org/), which stream and segment properly. Downloads work the same way (the file is pulled into memory before it is saved), so very large downloads are also CLI or rclone territory.

## Managing who can access a container

Access controls live at the container root (Swift permissions are per-container, never per-folder).

**Public access** is controlled from the **Actions** menu. Making a container public means **anyone with the link can download any object in it**, with no login — so only do it if you intend all of its current and future contents to be world-readable. Object *names* stay unlistable unless you also turn on listing (below). Making a container private again is confirmed the same way, because **existing public links stop working** once you do. Revocation only affects new requests — it cannot recall copies someone already downloaded.

An **Advanced sharing** section (collapsed by default) covers everything beyond the public toggle:

- **Who has access** — a plain-language list of the current grants, each removable.
- **Add people** — grant access to a specific person or a whole project. There is no directory to pick from, so you enter the collaborator's project ID (and, optionally, user ID — leave it blank to share with the whole project) and choose read-only or read-and-write.
- **Share with everyone in my project** — a one-click read grant for your own project.
- **List object names** — a separate, stronger step than public read: it lets anyone enumerate every object name in the container, not just fetch a key they already know. It is confirmed on its own.
- **Advanced ACL** — a power-user escape hatch to edit the raw read/write permission strings directly.

> **On some clouds** not every sharing rule is honored the same way. Object storage backends other than stock Swift (for example Ceph RADOS Gateway) support only a subset of these permissions, so an advanced rule may not take effect exactly as written.

## Connect with an S3 client

Exosphere browses your storage over the Swift API only; it does not speak the S3 protocol itself. But most OpenStack object storage is *also* reachable over an S3-compatible API, and the **Connect with an S3 client** panel helps you use it from tools like [rclone](https://rclone.org/) or Cyberduck.

When your cloud advertises an S3 endpoint, the panel shows the endpoint URL, the region value S3 clients expect, and your access key and secret key, plus a ready-to-paste rclone configuration. Each container is an S3 bucket of the same name. If you have no S3 credentials yet, **Create credentials** makes a pair for you. If your cloud does not offer an S3 API, the panel says so and suggests asking your cloud administrator.

The secret key is hidden until you reveal it, and copying the rclone snippet includes the real secret so it is ready to use.

## When object storage is not available

The Object Storage tile appears only when Experimental features is enabled in Settings and the service catalog provides an `object-store` endpoint. If the tile does not appear:

- check that **Experimental features** is enabled in Settings, and
- ask your cloud administrator whether the service catalog provides an `object-store` endpoint.

If there is no `object-store` endpoint, opening the Object Storage page directly shows a clean "not available" message rather than an error.

Nothing else in Exosphere is affected.

## What's covered, and what to use the CLI or rclone for

Exosphere covers everyday object storage. Some Swift capabilities are deliberately left to the OpenStack/Swift CLI or [rclone](https://rclone.org/).

| Capability | In Exosphere? | Alternative |
|---|---|---|
| Containers — create / list / delete | Yes | — |
| Objects — list / download / delete (single and bulk) | Yes | — |
| Pseudo-folders and breadcrumb navigation | Yes | — |
| Create an empty folder | Yes | — |
| Upload (up to 100 MiB per file) | Yes | CLI / rclone for larger |
| Copy / move objects (server-side) | Yes (single object) | CLI / rclone for large objects |
| Make a container public or private; share with people or a project | Yes | — |
| Copy public links | Yes | — |
| Multipart / large objects (SLO/DLO, over 5 GB), and large-object-safe cleanup | No | CLI / rclone |
| Object versioning | No | CLI / rclone |
| Object metadata editing | No | CLI / rclone |
| Static website hosting configuration | No | CLI / rclone |
| Account quotas and metadata | No | CLI / rclone |
| Signed temporary (TempURL) links | No | CLI / rclone |
| Browsing over the S3 protocol | No (Exosphere browses over Swift) | — |
| S3 client connection settings (endpoint, keys, rclone config) | Yes (when your cloud offers an S3 API) | — |

A note on large objects: copying, moving, or deleting a large object from the UI acts only on its manifest, not its underlying segments, so use the CLI or rclone to duplicate or fully remove them. The copy/move and delete confirmations remind you of this.

## For cloud operators

Every object storage request from the browser is a cross-origin request, so a deployment must satisfy the [same-origin policy](./solving-cors-problem.md) one of two ways:

1. **The cloud serves CORS.** The object storage service must return CORS headers for Exosphere's origin — including on **account-level** requests (the container listing). Some Swift versions do not serve CORS on account-level requests at all, so those clouds cannot list containers this way and must use the proxy below.
2. **The CORS proxy.** Route requests through Exosphere's proxy (see [Solving the CORS Problem](./solving-cors-problem.md)). The proxy must expose the Swift response headers this feature reads (for example `ETag`, `Content-Disposition`, `X-Container-Read`/`X-Container-Write`, `X-Container-Bytes-Used`) and raise its request body-size limit above nginx's default of 1 MiB so uploads succeed.
