import type { ColorSet } from "themer";

const sonokai: ColorSet = {
  // Color sets should provide a human-readable name.
  name: "onc",

  // Color sets can define a dark variant, a light variant, or both.
  // Each variant provides two or eight shades and eight accent colors in hex format.
  variants: {
    // In a dark variant, shade0 should be the darkest and shade7 should be
    // the lightest.
    //
    // base00: "#212121"
    // base01: "#424242"
    // base02: "#616161"
    // base03: "#757575"
    // base04: "#9E9E9E"
    // base05: "#EEEEEE"
    // base06: "#F5F5F5"
    // base07: "#FAFAFA"
    // base08: "#FF1744"
    // base09: "#00B0FF"
    // base0A: "#D500F9"
    // base0B: "#1DE9B6"
    // base0C: "#18FFFF"
    // base0D: "#FF9100"
    // base0E: "#FFEA00"
    // base0F: "#8D6E63"
    // 
    dark: {
      shade0: "#212121",
      shade7: "#FAFAFA",
      accent0: "#FF1744",
      accent1: "#FF9100",
      accent2: "#FFEA00",
      accent3: "#1DE9B6",
      accent4: "#18FFFF",
      accent5: "#00B0FF",
      accent6: "#FAFAFA",
      accent7: "#D500F9",
    },

    // In a light variant, shade7 should be the darkest and shade0 should be
    // the lightest.
    // light: {
    //   shade0: "#eeeeee",
    //   shade7: "#333333",
    //   accent0: "#f03e4d",
    //   accent1: "#f37735",
    //   accent2: "#eeba21",
    //   accent3: "#97bd2d",
    //   accent4: "#1fc598",
    //   accent5: "#53a6e1",
    //   accent6: "#bf65f0",
    //   accent7: "#ee4eb8",
    // },
  },
};

export default sonokai;