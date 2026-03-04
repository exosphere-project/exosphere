import dotenv from "dotenv";
import { defineConfig } from "playwright/test";

// Load environment variables from .env.
dotenv.config();

export default defineConfig({
  outputDir: "results",
  webServer: {
    // TODO: Serve the production build instead of the development server for more accurate performance testing.
    command: "npm run start",
    url: "http://app.exosphere.localhost:8000/",
    reuseExistingServer: true,
  },
  // Note: Switch away from headless mode to debug or follow the test.
  // use: {
  //   headless: false,
  // },
});
