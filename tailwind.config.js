/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./index.html", "./src/**/*.{js,ts,jsx,tsx,elm}"],
  theme: {
    extend: {
      fontFamily: {
        gochi: ["Gochi Hand", "cursive"],
        medieval: ["MedievalSharp", "cursive"],
      },
    },
  },
  plugins: [],
};
