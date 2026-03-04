import path from "node:path";
import { test as setup } from "playwright/test";

// Destination for saving the logged in user state after signing in.
//  (This file can be loaded in other tests to skip authentication.)
const authFile = path.join(import.meta.dirname, "./.auth/user.json");

const OS_AUTH_URL = process.env.OS_AUTH_URL;
const OS_DOMAIN = process.env.OS_DOMAIN;
const OS_USERNAME = process.env.OS_USERNAME;
const OS_PASSWORD = process.env.OS_PASSWORD;
const OS_PROJECT = process.env.OS_PROJECT;
const OS_REGION = process.env.OS_REGION;

setup("authenticate", async ({ page }) => {
  // Increase the default timeout.
  setup.slow();

  await page.goto("http://app.exosphere.localhost:8000/loginpicker");

  await page.getByRole("button", { name: "Add OpenStack Account" }).click();
  await page
    .getByRole("textbox", { name: "Keystone auth URL OS_AUTH_URL" })
    .click();
  await page
    .getByRole("textbox", { name: "Keystone auth URL OS_AUTH_URL" })
    .fill(OS_AUTH_URL);
  await page
    .getByRole("textbox", { name: "User Domain (name or ID) User" })
    .click();
  await page
    .getByRole("textbox", { name: "User Domain (name or ID) User" })
    .fill(OS_DOMAIN);
  await page
    .getByRole("textbox", { name: "User Name User name e.g. demo" })
    .click();
  await page
    .getByRole("textbox", { name: "User Name User name e.g. demo" })
    .fill(OS_USERNAME);
  await page.getByRole("textbox", { name: "Password Password" }).click();
  await page
    .getByRole("textbox", { name: "Password Password" })
    .fill(OS_PASSWORD);
  await page.getByRole("button", { name: "Log In" }).click({ timeout: 1000 });
  await page.getByRole("checkbox", { name: `${OS_PROJECT}` }).click();
  await page.getByRole("button", { name: "Choose" }).click();
  await page.getByRole("checkbox", { name: `${OS_REGION}` }).click();
  await page.getByRole("button", { name: "Choose" }).click();

  // Wait for the page to load.
  await page.waitForLoadState("domcontentloaded");

  // Save the storage state.
  await page.context().storageState({ path: authFile });
});
