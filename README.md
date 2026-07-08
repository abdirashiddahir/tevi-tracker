# Coming Soon Tracker — Shiny app (bslib)

INDOT-themed companion to the Indiana NEVI Dashboard for verifying and tracking the
**Coming Soon** charging stations against PlugShare. Design language adopted from
`NEVI_National_Dashboard` (bslib / Bootstrap 5, navy + gold, Inter, value boxes, gold-accented
cards). Shared, persistent (SQLite-backed) edits **plus a live PlugShare auto-updater**.

## Run locally
```r
shiny::runApp("Coming Soon Tracker App")
```
Packages: `shiny, bslib, bsicons, leaflet, DT, dplyr, DBI, RSQLite, maps, sf, httr2, shinycssloaders`
(`sf` reads the official Indiana boundary shapefile; if `sf` or the shapefile is missing the app
falls back to the `maps` outline.)
(all already installed for this project).

## Tabs
- **Map** — value-box KPIs (Tracked / Operational / Coming Soon / Under Repair / Not on PS) over an
  ESRI basemap (Streets / Topo / Imagery) with **Indiana (navy) + border-state (gray) outlines via
  the `maps` package**. Markers colored by status; **each popup shows the full PlugShare detail**
  (status, network, plugs, chargers, location ID, verified date, notes) + an
  **"Open full PlugShare listing ↗"** deep link.
- **Tracker** — editable table (PlugShare status, Operational, Network, Chargers, Notes).
  **Every edit saves to SQLite and is shared across users.** Export CSV.
- **Review** — the queue of stations the live check flagged as *possibly* operational. Each card has
  an *Investigate on PlugShare ↗* link plus **Confirm operational** / **Still coming soon** buttons.
  This is the only place a station becomes Operational. Map popups are enterprise SVG cards.
- **PlugShare** — opens zoomed to **all stations + the Indiana boundary**. **Click a station on the
  map** → an on-map PlugShare-style popup opens (PlugShare blocks iframes, so it's recreated) and the
  full detail + live link appears in the left panel, with a per-station **"Check live status"**. No
  dropdown — it's click-driven like the PlugShare website. The Indiana boundary is drawn from the
  official **`Indiana_State_Boundary_2020`** shapefile (via `sf`), not the `maps` package.
- **About** — data sources + how the live auto-update works.

## Live PlugShare check → human review (no silent auto-flips)
The sidebar **"Check all stations now"** (and the per-station / Review-tab buttons) fetches each
station's PlugShare page and reads its `og:title`. The reader is **conservative**: it returns
`coming_soon`, `candidate`, or `inconclusive`. A *specific* listing title with no "(Coming Soon)"
is a **candidate** → the station is **flagged for review (amber)**; a generic/blank PlugShare
homepage title is **inconclusive** and ignored (this prevents the earlier Wawa-type false positive).

**Nothing is promoted automatically.** Flagged stations appear on the **Review** tab with an
*Investigate on PlugShare ↗* link and **Confirm operational** / **Still coming soon** buttons. Only
**Confirm** sets a station Operational (via `confirm_operational()`); **Dismiss** clears the flag.
Promote confirmed flips to `master_Data.csv` afterwards. No PlugShare API key required.

> The authoritative status fields (`coming_soon`, `status:"OPERATIONAL"`, live charger power) live
> behind PlugShare's **authenticated** JSON API (401 without a paid key) — see the captured
> `plugshare_raw_*.json` and `HOW_IT_WORKS.html` §7. If a key is obtained later, the title scrape can
> be swapped for those exact fields and confirmation could be automated with high confidence.

A full, plain-language explainer (**`HOW_IT_WORKS.html`** — how it's built, how to use it, the
database for newcomers, the PlugShare JSON schema, a line-by-line code walkthrough, and Posit Connect
+ private-repo deployment) is kept in the **parent folder** as an internal reference; it is not part of
the deployed app.

## Deploy free to shinyapps.io
A Shiny app needs a real R server — it **cannot** run on Vercel/GitHub Pages (static only).
shinyapps.io is Posit's free tier and keeps the shared edits + live auto-updater.

1. Get a free account at https://www.shinyapps.io → **Account → Tokens → Show** (copies your
   `setAccountInfo(...)` line).
2. In R:
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name = "<acct>", token = "<token>", secret = "<secret>")
   rsconnect::deployApp(
     appDir   = "Coming Soon Tracker App",
     appName  = "coming-soon-tracker")
   ```
   (Or open `app.R` in RStudio → blue **Publish** button → choose shinyapps.io.)
3. **Persistence note:** on the shinyapps.io free tier the container's disk resets when the app goes
   to sleep, so SQLite edits are not guaranteed to persist long-term. For durable shared edits,
   either upgrade the plan, set env var `TRACKER_DB` to a mounted volume (Connect/HF Spaces), or
   point it at an external database. For a daily unattended auto-check, schedule a separate job
   (the app's "Check now" button covers on-demand checks).

Alternative free hosts that keep all features: **Hugging Face Spaces** (Docker) or
**Posit Connect Cloud**.

## Folder layout
```
Coming Soon Tracker App/
├── app.R                 # bslib UI + server + PlugShare reader + review queue
├── .gitignore            # excludes secrets + the runtime SQLite
├── Indiana_State_Boundary_2020/   # official INDOT boundary shapefile (.shp/.shx/.dbf/.prj) — committed
├── www/
│   ├── styles.css        # INDOT theme on bslib + PlugShare panel + Review cards
│   └── scripts.js
├── data/
│   ├── master_Data.csv                 # bundled copy (deploy fallback)
│   ├── verifications.R                 # bundled copy (deploy fallback)
│   └── tracker_store.sqlite            # created at runtime (shared edits) — gitignored
└── README.md
```

**Committing to GitHub:** the `.gitignore` already excludes `*.sqlite`, `credentials.R`, and dev
scratch files. `credentials.R` lives in the **project root** (one level up), so committing only this
app folder won't include it — but never commit it. The runtime `tracker_store.sqlite` is recreated on
first launch (it seeds from `master_Data.csv` + `verifications.R`), so it should not be committed.
