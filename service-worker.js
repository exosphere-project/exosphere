// Minimal service worker per
// https://stackoverflow.com/questions/61208907/pwa-minimal-service-worker-just-for-trigger-the-install-button
// Not intended to do anything useful yet, just enable Chrome/Chromium's "add to home screen" prompt

self.addEventListener("fetch", function (event) {
  event.respondWith(fetch(event.request));
});
