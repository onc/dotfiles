import themer from "themer";
import { writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, join } from "path";
import sonokai from "./sonokai.js";
import onc from "./onc.js";


const main = async () => {
  const files = themer(
    [sonokai, onc],
    ["vim", "iterm", "emacs"],
    { wallpaperSizes: [{ w: 1440, h: 900 }] }
  );

  for await (const file of files) {
    const exportDir =  join('themes', dirname(file.path));
    
    if (!existsSync(exportDir)) {
      mkdirSync(exportDir, { recursive: true });
    }

    console.log("Writing file", file.path);
    writeFileSync(join('themes', file.path), file.content);
  }
}

main();