import fs from "node:fs/promises";
import path from "node:path";
import { expect, test } from "playwright/test";

// Path of the saved logged-in user state.
const authFile = path.join(import.meta.dirname, "./.auth/user.json");
try {
  await fs.access(authFile);
  console.log("Authentication state file found.");
} catch {
  console.error("Authentication state file not found.");
}

test("whether the user is logged in", async ({ browser }) => {
  // Launch a new browser context with the authentication state.
  const context = await browser.newContext({
    storageState: authFile,
  });
  const page = await context.newPage();
  await page.goto("http://app.exosphere.localhost:8000/");

  // Wait for the page to render.
  await page.waitForLoadState("domcontentloaded");

  // Capture a screenshot of the page.
  await page.screenshot({
    fullPage: true,
    path: path.join(import.meta.dirname, "./screenshots/check.jpg"),
  });

  // Check for the presence of a named project.
  await expect(page.getByRole("link", { name: /^Project\s.+/ })).toBeVisible();
});
