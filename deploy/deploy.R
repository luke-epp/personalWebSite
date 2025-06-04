# Install required packages if not already installed
if (!require("quarto")) install.packages("quarto")
if (!require("rmarkdown")) install.packages("rmarkdown")

# Render the website
quarto::quarto_render()

# Message about deployment
cat("Website has been built in the 'docs' directory.\n")
cat("To deploy to GitHub Pages:\n")
cat("1. Commit all changes\n")
cat("2. Push to your GitHub repository\n")
cat("3. Enable GitHub Pages in your repository settings\n")
cat(
  "   (Settings -> Pages -> Source -> select 'main' branch and '/docs' folder)\n"
)
