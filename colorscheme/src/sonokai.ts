import type { ColorSet } from "themer";

const sonokai: ColorSet = {
  // Color sets should provide a human-readable name.
  name: "onc-sonokai",

  // Color sets can define a dark variant, a light variant, or both.
  // Each variant provides two or eight shades and eight accent colors in hex format.
  variants: {
    // In a dark variant, shade0 should be the darkest and shade7 should be
    // the lightest.
    dark: {
      shade0: "#2C2E34",
      // Note: you can define shades 1 through 6 yourself, or you can omit
      // them; if omitted, they will be calculated automatically by
      // interpolating between shade0 and shade7.
      shade7: "#E2E2E3",
      accent0: "#FC5D7C",
      accent1: "#F39660",
      accent2: "#E7C664",
      accent3: "#9ED072",
      accent4: "#8ACEA9",
      accent5: "#76CCE0",
      accent6: "#B39DF3",
      accent7: "#BAA6F4",
    },

    // In a light variant, shade7 should be the darkest and shade0 should be
    // the lightest.
    light: {
      shade0: "#eeeeee",
      shade7: "#333333",
      accent0: "#f03e4d",
      accent1: "#f37735",
      accent2: "#eeba21",
      accent3: "#97bd2d",
      accent4: "#1fc598",
      accent5: "#53a6e1",
      accent6: "#bf65f0",
      accent7: "#ee4eb8",
    },
  },
};

export default sonokai;