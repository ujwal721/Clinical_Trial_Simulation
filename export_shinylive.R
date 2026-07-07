# Run this script from the repository root in RStudio.
# It creates docs/ for GitHub Pages, with:
#   docs/index.html      -> landing page
#   docs/dashboard/      -> browser-only Shiny app

required <- c("shinylive", "httpuv")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

if (!file.exists("shinylive_app/app.R")) {
  stop("Run this script from the repository root. shinylive_app/app.R was not found.")
}

if (dir.exists("docs/dashboard")) unlink("docs/dashboard", recursive = TRUE)
dir.create("docs", recursive = TRUE, showWarnings = FALSE)
dir.create("docs/assets", recursive = TRUE, showWarnings = FALSE)

file.copy("index.html", "docs/index.html", overwrite = TRUE)
file.copy("styles.css", "docs/styles.css", overwrite = TRUE)
file.copy("assets/boin-dashboard-preview.png", "docs/assets/boin-dashboard-preview.png", overwrite = TRUE)
writeLines("", "docs/.nojekyll")

shinylive::export("shinylive_app", "docs/dashboard")

message("\nExport finished. Preview locally with:\n")
message('httpuv::runStaticServer("docs", port = 8008)')
message("\nThen open http://127.0.0.1:8008/ in your browser.")
message("\nWhen it works, commit docs/ and set GitHub Pages to main /docs.")
