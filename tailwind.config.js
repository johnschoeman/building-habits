module.exports = {
  theme: {
    extend: {
      transitionProperty: {
        height: "height",
      },
      height: {
        80: "20rem",
      },
      gridTemplateColumns: {
        "2-1": "2fr 1fr",
        "3-1": "3fr 1fr",
      },
      flex: {
        auto: "1 1 auto",
        initial: "0 1 auto",
        inherit: "inherit",
        none: "none",
        2: "2 2 0%",
        3: "3 3 0%",
        4: "4 4 0%",
        5: "5 5 0%",
      },
      minHeight: {
        16: "4rem",
      },
      maxHeight: {
        32: "8rem",
        64: "16rem",
        screen: "100vh",
        full: "100%",
      },
    },
    variants: {},
    plugins: [],
  },
};
