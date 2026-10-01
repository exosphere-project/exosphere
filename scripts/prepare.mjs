import os from "os";
import process from "child_process";
import fs from "fs";
import path from "path";
import util from "util";

const exec = util.promisify(process.exec);

async function commandExists(command) {
  try {
    if (os.platform() == "win32") {
      await exec(`where ${command}`);
    } else {
      await exec(`command -v ${command}`);
    }
    return true;
  } catch (error) {
    return false;
  }
}

async function main() {
  if (!(await commandExists("pre-commit"))) {
    console.log("pre-commit is not installed, see contributing.md");
    return;
  }

  if (!fs.existsSync(path.join(".git", "hooks", "pre-commit"))) {
    const { stdout } = await exec("pre-commit install", { encoding: "utf8" });
    console.log(stdout);
  }
}

main().catch((error) => {
  console.log(error.stdout || error.message);
  console.log("pre-commit install failed, see contributing.md");
});
