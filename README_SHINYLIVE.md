# Convert this repository to a browser-only Shinylive dashboard

This update keeps the portfolio landing page and adds a browser-only dashboard at:

```text
https://ujwal721.github.io/Clinical_Trial_Simulation/dashboard/
```

## Why this version is different

The original server-hosted app uses `plotly`, `DT`, and optional Gemini API code. The browser-only version deliberately uses only `shiny` and base R graphics, so no API key, R server, or active-hour quota is required. The original project code contains those server-only dependencies and optional Gemini logic. The Shinylive version removes them rather than exposing an API key.

## Install into your existing repository

1. Copy the contents of this update into the root of your local repository.
2. In RStudio, open the repository folder and run the whole `export_shinylive.R` script from the Console:

```r
source("export_shinylive.R")
```

3. Wait for export to finish. The first export downloads the Shinylive/webR assets and may take a few minutes.
4. Preview the static site locally:

```r
httpuv::runStaticServer("docs", port = 8008)
```

Open `http://127.0.0.1:8008/`. Click **Launch interactive dashboard**.

5. Commit and push the generated `docs/` folder:

```bash
git add shinylive_app export_shinylive.R index.html README_SHINYLIVE.md docs
git commit -m "Add browser-only Shinylive dashboard"
git push
```

6. On GitHub: **Settings → Pages → Deploy from a branch → `main` → `/docs` → Save.**

GitHub Pages will publish the landing page at the root and the interactive dashboard under `/dashboard/`.

## Important limitations

- There is no Gemini/AI tab in this version. A browser app cannot safely hide a private API key.
- Computation happens in the visitor's browser, so the first visit can take a little longer and very large simulation counts should be avoided.
- This remains an educational simulation, not validated clinical decision software.
