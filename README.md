# TEVI Tracker — Tennessee NEVI (Shiny / bslib)

A self-service tracker for Tennessee's NEVI‑funded and other DC fast‑charging stations. Unlike the
Indiana "Coming Soon" tool it was templated from, this tracks **all station types** — Coming Soon,
NEVI Awarded, and Open (Creditable) — so staff can see *when each site opens*. Existing (non‑NEVI)
DCFCs are shown as **context only** (toggle, off by default) and are **not tracked**. TDOT / EPIC
branding (navy + red), Alternative Fuel Corridor overlay, and durable SQLite‑backed edits.

## Run locally
```r
shiny::runApp(".", port = 7788)   # from inside "Coming Soon Tracker App - TN"
```
Login (defaults, overridable via env vars): user `TEVItracker`, password `TEVI_HNTB2026?!_94`.

Packages: `shiny, bslib, bsicons, leaflet, DT, dplyr, DBI, RSQLite, maps, sf, httr2`
(all already installed for this project). The Tennessee outline uses the `maps` package; drop a
`Tennessee_State_Boundary/` shapefile in the app folder to use an official boundary instead.

## Data
`data/master_Data_TN.csv` — 182 rows, one unified schema. Stations are classified by the
`data_source` column:

| `data_source`        | Shown as        | Tracked? | PlugShare link |
|----------------------|-----------------|----------|----------------|
| `Coming_Soon`        | Coming Soon      | yes      | yes            |
| `NEVI Awarded Sites` | NEVI Awarded     | yes      | yes (RA‑added) |
| `Open_Creditable`    | Open (Creditable)| yes      | yes            |
| `Other_DCFC`         | Existing DCFC    | **no** (context) | mixed  |

The **Tracked** KPI and the PlugShare tab exclude `Other_DCFC` by default. All 28 NEVI Awarded sites
now carry a PlugShare `location/<id>` link (added by a research assistant via
`TEVI_Awarded_Missing_PlugShare.xlsx`, then merged into the CSV by coordinate match).

## Tabs
- **Map** — KPI value boxes (Tracked / Operational / Coming Soon / New CS / Needs Review / Awarded /
  Existing DCFC) over an ESRI basemap, with the TN outline + border‑state lines. Markers colored by
  status; **Alternative Fuel Corridors** (FHWA, live‑fetched, `STATE='TN'`) draw as a toggleable
  orange overlay.
- **Tracker** — editable table with a **layer dropdown** (All / Coming Soon / Awarded / Open /
  Existing DCFC). Every edit saves to SQLite. Export CSV.
- **Review** — the queue of stations the live check flagged as *possibly* operational; each card
  shows **which layer** it belongs to and has **Confirm operational** / **Still coming soon** buttons.
  This is the only place a station becomes Operational.
- **Add station** — add a station of any type with a confidence level; edits/deletes persist.
- **PlugShare** — click a station → an on‑map PlugShare‑style popup + full detail in the side panel.
- **About** — data sources, the lifecycle model, and map‑color legend.

## Alternative Fuel Corridors
Fetched at startup from the FHWA ArcGIS FeatureServer
(`AltFuelCorridors_R1to7_WGS84_Public_View`) filtered to `STATE='TN'` → I‑24, I‑26, I‑40, I‑65,
I‑75, I‑81, US‑64. Wrapped in `tryCatch`: if the service is unreachable the app still starts, just
without the corridor overlay. (ArcGIS **web‑map write** integration is a planned later phase.)

## Deployment — private GitHub repo → Posit Connect Cloud

A Shiny app needs a real R server (it can't run on Vercel/GitHub Pages). We deploy to **Posit
Connect Cloud** (org **HNTB Tech**, `connect.posit.cloud/hntboh`), which builds directly from a
GitHub repository using the committed `manifest.json`.

**Secrets:** keep the repo **private** — `app.R` contains the login defaults. `.gitignore` already
excludes `credentials.R`, `*.xlsx`, the `TEVI reference materials/` folder, the runtime
`*.sqlite`, and dev scratch files. Never commit an API token; set secrets as Connect env vars.

### A. Create the private repo & push
1. On GitHub → **New repository**, e.g. `tevi-tracker`, **Visibility: Private**. Do **not** add a
   README / .gitignore / license (this repo already has them).
2. Push:
   ```bash
   git remote add origin https://github.com/<ACCOUNT>/<REPO>.git
   git push -u origin master        # or: git branch -M main && git push -u origin main
   ```

### B. Publish on Posit Connect Cloud
1. At `connect.posit.cloud/hntboh` → **Publish** → **Shiny**.
2. Authorize the **Posit Connect Cloud GitHub App** for the new private repo (Configure → grant
   access to just that repo).
3. Select **Repository** = the new repo, **Branch** = `master` (or `main`), **Primary file** =
   `app.R`, then **Publish**. First build takes a few minutes (`sf` / `leaflet` compile).

### C. Set credentials as environment variables (recommended)
In the deployed content → **⋮ → Settings → Variables**:

| Variable          | Value                                            |
|-------------------|--------------------------------------------------|
| `TRACKER_USER`    | `TEVItracker`                                     |
| `TRACKER_PASS`    | `TEVI_HNTB2026?!_94`                               |
| `STATUS_API_URL`  | *(leave empty — API writes stay no‑ops until the ArcGIS phase)* |

Then **Redeploy** so the variables take effect.

### Regenerating the manifest
If dependencies change, rebuild `manifest.json` before pushing:
```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

## Updating PlugShare links (research‑assistant workflow)
1. Send `TEVI_Awarded_Missing_PlugShare.xlsx` (gitignored) to the RA to fill the `PlugShare_link`
   column.
2. Merge the completed workbook into `data/master_Data_TN.csv` **by coordinate** (station names
   repeat across chains, so lat/lon is the safe key). Change only `PlugShare_link`; preserve every
   other cell (including literal `NA` tokens) verbatim.
3. Relaunch and confirm each updated row yields a numeric `location/<id>`.

## Folder layout
```
Coming Soon Tracker App - TN/
├── app.R                     # bslib UI + server + PlugShare reader + AFC overlay + review queue
├── manifest.json             # Posit Connect Cloud dependency lockfile (app.R entrypoint)
├── .gitignore                # excludes secrets, xlsx, reference materials, runtime SQLite
├── www/
│   ├── styles.css            # TDOT/EPIC theme (navy + red)
│   ├── scripts.js
│   ├── TDOT_logo.png
│   └── hntb_logo.png
├── data/
│   └── master_Data_TN.csv    # unified station dataset (182 rows)
├── TN_City_Boundaries/       # optional city shapefile (state outline falls back to {maps})
└── README.md
```
