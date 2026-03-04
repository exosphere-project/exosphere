import lighthouse from "lighthouse";
import fs from "node:fs/promises";
import playwright from "playwright";
import { test } from "playwright/test";
import state from "./.auth/user.json" with { type: "json" };
import { rimraf } from "rimraf";

const performenceTestDir = "./performance-tests";
const browserCacheDir = `${performenceTestDir}/browser`;
const reportsDir = `${performenceTestDir}/reports`;
const remoteDebuggingPort = 9222;

const URL = process.env.URL || "http://app.exosphere.localhost:8000";

test.describe("performance audit", () => {
  // Increase the default timeout.
  test.slow();

  test("home when signed in", async () => {
    const context = await playwright.chromium.launchPersistentContext(
      browserCacheDir,
      {
        args: [`--remote-debugging-port=${remoteDebuggingPort}`],
      }
    );

    // Store the logged in user state in a persistent browser context.
    const page = await context.newPage();
    await page.goto(URL);
    await page.evaluate((injectedState) => {
      // Manually save the logged in state to local storage.
      console.log("Setting local storage for performance test...");
      const storedState = injectedState?.origins?.[0]?.localStorage;
      for (const item of storedState) {
        localStorage.setItem(item.name, item.value);
      }
      console.log("Local storage set for performance test √");
    }, state);

    // Run the Lighthouse performance audit.
    const formats = ["html", "csv"];
    const result = await lighthouse(
      URL,
      {
        port: remoteDebuggingPort,
        output: formats,
        logLevel: "verbose",
      },
      {
        extends: "lighthouse:default",
        settings: {
          disableStorageReset: true, // Keep local storage
          formFactor: "desktop", // Desktop
          screenEmulation: { disabled: true }, // Desktop
          throttling: {
            // Lighthouse default throttling:
            //  https://github.com/GoogleChrome/lighthouse/blob/main/docs/throttling.md
            //  Throughput: 1.6Mbps down / 750 Kbps up | Latency: 150ms
            downloadThroughputKbps: 1600,
            uploadThroughputKbps: 750,
            // This latency drives the "First Contentful Paint" performance.
            //  150ms is the default & results in an unrealistic FCP of 18s.
            //  1ms results in a realistic FCP of 3s (comparable to running Lighthouse in the browser).
            rttMs: 1,
          },
        },
      },
      undefined // Page does not support Playwright.
    );

    // Save the Lighthouse report.
    const pad = (num) => num.toString().padStart(2, "0");
    const now = new Date();
    const timestamp = `${now.getFullYear()}${pad(now.getMonth() + 1)}${pad(now.getDate())}_${pad(now.getHours())}${pad(now.getMinutes())}`;
    for (const format of formats) {
      const report = result.report[formats.indexOf(format)];
      const reportPath = `${reportsDir}/lighthouse-report-${timestamp}.${format}`;
      await fs.writeFile(reportPath, report);
    }

    // Clean up the browser context.
    await context.close();
  });
});

test.afterAll(async () => {
  console.log("Cleaning up browser cache...");

  // Delete the persistent context directory.
  await rimraf(browserCacheDir);
});
