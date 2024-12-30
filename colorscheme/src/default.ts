import themer from "themer";
import { writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname } from "path";
import sonokai from "./sonokai.js";


const main = async () => {
  const files = themer(
    [sonokai],
    ["vim", "iterm", "emacs"],
    { wallpaperSizes: [{ w: 1440, h: 900 }] }
  );

  for await (const file of files) {
    const exportDir = dirname(file.path);
    if (!existsSync(exportDir)) {
      mkdirSync(exportDir, { recursive: true });
    }

    writeFileSync(file.path, file.content);
  }
}

main();