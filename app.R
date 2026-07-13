# =============================================================================
# TEVI Tracker — Tennessee NEVI — Shiny (bslib) app
# TDOT/EPIC-branded (navy + red) tracker for ALL Tennessee station types:
# Coming Soon, NEVI Awarded, and Open (Creditable); existing DCFCs shown as
# context only. Includes an FHWA Alternative Fuel Corridor overlay. SQLite-backed
# tracking + live PlugShare auto-update (scrapes each location ID's og:title to
# detect status changes). Templated from the Indiana Coming Soon Tracker.
# =============================================================================
library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(DT)
library(dplyr)
library(DBI)
library(RSQLite)
library(maps)
suppressWarnings(suppressMessages(if (requireNamespace("sf", quietly = TRUE)) library(sf)))
library(httr2)
library(htmltools)
suppressWarnings(library(shinycssloaders))

# ---- file resolution + DB ---------------------------------------------------
find_file <- function(name) {
  for (p in c(file.path("..", name), name, file.path("data", name)))
    if (file.exists(p)) return(p)
  stop(paste("Required file not found:", name))
}
if (!dir.exists("data")) dir.create("data")
DB_PATH <- Sys.getenv("TRACKER_DB", file.path("data", "tracker_store.sqlite"))

# Shared status API (Render) — defined up here (before build_base) because we also
# re-hydrate user-added stations FROM the API at startup. Never hard-code; env vars:
#   STATUS_API_URL    e.g. https://nevi-status-api.onrender.com
#   STATUS_API_TOKEN  the shared write secret (same value set on the Render API)
STATUS_API_URL   <- Sys.getenv("STATUS_API_URL",   "")
STATUS_API_TOKEN <- Sys.getenv("STATUS_API_TOKEN", "")

# TDOT / EPIC palette: navy + red are the primary brand colors; the rest stay functional.
INDOT <- list(navy = "#00205B", navy_dark = "#00163E", gold = "#EF3E33",   # EPIC navy + red accent
              green = "#2E7D32", teal = "#17A2B8", red = "#DC3545", gray = "#75787B",
              amber = "#E8910C", newcs = "#7E57C2",   # violet = user-added "New Coming-Soon"
              brown = "#8D6E63")                       # Not-on-PlugShare (frees gray for Low confidence)

# ---- Access credentials (shared login gate) --------------------------------
# A single shared username/password gates the whole app. Defaults are provided so it
# works out of the box, but on Posit Connect Cloud set TRACKER_USER / TRACKER_PASS in
# Settings -> Vars to override them WITHOUT putting the password in the public repo.
AUTH_USER <- Sys.getenv("TRACKER_USER", "TEVItracker")
AUTH_PASS <- Sys.getenv("TRACKER_PASS", "TEVI_HNTB2026?!_94")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

slugify <- function(x) tolower(gsub("(^-|-$)", "", gsub("[^A-Za-z0-9]+", "-", trimws(x))))

# ---- Custom (user-added) stations -------------------------------------------
# Stations entered through the "Add Station" form are stored in their OWN table so they
# survive restarts, then folded into the station universe by build_base(). From that
# point a custom station behaves exactly like a master_Data.csv Coming Soon site: shown
# on the map, scanned by the live check, and flagged for review when it goes operational.
CUSTOM_COLS <- c("station_id","station_name","address","state","lat","lon",
                 "location_id","network","plugs","chargers","open_date","notes","added_at",
                 "data_source","confidence_level")
ensure_custom_table <- function(con) {
  if (!dbExistsTable(con, "custom_stations")) {
    empty <- as.data.frame(setNames(rep(list(character(0)), length(CUSTOM_COLS)), CUSTOM_COLS),
                           stringsAsFactors = FALSE)
    dbWriteTable(con, "custom_stations", empty)
  } else {                                   # migrate older local tables to new columns
    flds <- dbListFields(con, "custom_stations")
    for (cc in setdiff(CUSTOM_COLS, flds))
      dbExecute(con, sprintf("ALTER TABLE custom_stations ADD COLUMN %s TEXT DEFAULT ''", cc))
  }
}
read_custom_stations <- function() {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  ensure_custom_table(con); dbReadTable(con, "custom_stations")
}
add_custom_station <- function(rec) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  ensure_custom_table(con); dbAppendTable(con, "custom_stations", rec[, CUSTOM_COLS])
}
custom_exists <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con)); ensure_custom_table(con)
  nrow(dbGetQuery(con, "SELECT 1 FROM custom_stations WHERE station_id = ? LIMIT 1",
                  params = list(id))) > 0
}
# Remove a custom station entirely: from its own table AND the tracking overlay.
delete_custom_station <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM custom_stations WHERE station_id = ?", params = list(id))
  dbExecute(con, "DELETE FROM tracking WHERE station_id = ?",        params = list(id))
}
# Update an existing custom station's editable fields (used by the Edit dialog).
update_custom_station <- function(id, f) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE custom_stations SET station_name=?, state=?, lat=?, lon=?,
                  location_id=?, network=?, plugs=?, chargers=?, open_date=?, notes=?,
                  data_source=?, confidence_level=? WHERE station_id=?",
            params = list(f$station_name, f$state, f$lat, f$lon, f$location_id, f$network,
                          f$plugs, f$chargers, f$open_date, f$notes,
                          f$data_source %||% "", f$confidence_level %||% "", id))
  # keep the tracking overlay's mutable fields in sync with the edit
  dbExecute(con, "UPDATE tracking SET network=?, plugs=?, chargers=?, notes=?, updated_at=? WHERE station_id=?",
            params = list(f$network, f$plugs, f$chargers, f$notes, as.character(Sys.time()), id))
}
# Insert a fresh tracking row for a runtime-added station (Coming Soon, no review flag),
# so the merged() left-join has its mutable fields immediately — without a restart.
seed_tracking <- function(rec) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  row <- data.frame(station_id = rec$station_id, ps_status = "Coming Soon", operational = "",
    network = rec$network, plugs = rec$plugs, chargers = rec$chargers, notes = rec$notes,
    verified = "", verified_date = "", updated_at = as.character(Sys.time()),
    last_checked = "", review_flag = "", review_at = "", stringsAsFactors = FALSE)
  dbAppendTable(con, "tracking", row)
}
# Map a custom_stations data.frame onto the BASE column layout used everywhere else.
custom_to_base <- function(cu) {
  if (is.null(cu) || nrow(cu) == 0) return(NULL)
  # Only Coming Soon customs belong on the Tracker's map/review/live-check flow. Other
  # types (NEVI Awarded, etc.) are data-entry passthrough to the Scenario tool — they
  # stay in the registry (managed via the Recently Added table) but skip this universe.
  ds0 <- ifelse(is.na(cu$data_source) | cu$data_source == "", "Coming_Soon",
                as.character(cu$data_source))
  cu  <- cu[ds0 == "Coming_Soon", , drop = FALSE]
  if (nrow(cu) == 0) return(NULL)
  nz  <- function(x) ifelse(is.na(x), "", as.character(x))
  loc <- trimws(nz(cu$location_id))
  data.frame(
    station_id = cu$station_id, station_name = nz(cu$station_name), address = nz(cu$address),
    state = nz(cu$state),
    lat = suppressWarnings(as.numeric(cu$lat)), lon = suppressWarnings(as.numeric(cu$lon)),
    location_id = loc,
    plugshare_url = ifelse(nzchar(loc), paste0("https://www.plugshare.com/location/", loc), ""),
    ps_status = "Coming Soon", operational = "", master_operational = FALSE,
    network = nz(cu$network), plugs = nz(cu$plugs), chargers = nz(cu$chargers),
    notes = nz(cu$notes), open_date = nz(cu$open_date), verified = "",
    data_source = nz(cu$data_source), confidence_level = nz(cu$confidence_level), is_custom = TRUE,
    stringsAsFactors = FALSE)
}

# ---- Custom-station API client (the DURABLE store on the Render disk) ---------
# The local SQLite is wiped whenever Connect Cloud restarts the container, so the
# authoritative copy of user-added stations lives on the Render API's persistent
# disk. The Tracker WRITES through to the API on save/edit/delete, and on startup
# RE-HYDRATES its local table from the API — so added stations survive restarts and
# are shared across everyone. Every call is a safe no-op if the API is unreachable.
api_list_stations <- function() {
  if (!nzchar(STATUS_API_URL)) return(NULL)
  tryCatch({
    resp <- request(paste0(STATUS_API_URL, "/stations")) |> req_timeout(15) |> req_perform()
    st <- resp_body_json(resp, simplifyVector = FALSE)$stations
    if (is.null(st) || length(st) == 0) return(data.frame())
    do.call(rbind, lapply(st, function(s)
      as.data.frame(lapply(s, function(v) if (is.null(v)) "" else as.character(v)),
                    stringsAsFactors = FALSE)))
  }, error = function(e) { message("Station API list failed: ", conditionMessage(e)); NULL })
}
api_save_station <- function(rec) {                       # add or update (upsert)
  if (!nzchar(STATUS_API_URL)) return(invisible(FALSE))
  tryCatch({
    request(paste0(STATUS_API_URL, "/stations")) |>
      req_method("POST") |> req_headers(`X-API-Token` = STATUS_API_TOKEN) |>
      req_body_json(as.list(rec[1, ])) |> req_timeout(15) |> req_perform()
    TRUE
  }, error = function(e) { message("Station API save failed: ", conditionMessage(e)); FALSE })
}
api_delete_station <- function(id) {
  if (!nzchar(STATUS_API_URL)) return(invisible(FALSE))
  tryCatch({
    request(paste0(STATUS_API_URL, "/stations")) |>
      req_method("DELETE") |> req_headers(`X-API-Token` = STATUS_API_TOKEN) |>
      req_body_json(list(station_id = id)) |> req_timeout(15) |> req_perform()
    TRUE
  }, error = function(e) { message("Station API delete failed: ", conditionMessage(e)); FALSE })
}
# Mirror the durable API list into the local table at startup (API is the source of
# truth). If the API is unreachable, keep whatever is local — never wipe blindly.
sync_custom_from_api <- function() {
  remote <- api_list_stations()
  if (is.null(remote)) return(invisible(FALSE))           # unreachable -> leave local as-is
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con)); ensure_custom_table(con)
  dbExecute(con, "DELETE FROM custom_stations")
  if (nrow(remote) > 0) {
    keep <- intersect(CUSTOM_COLS, names(remote))
    dbAppendTable(con, "custom_stations", remote[, keep, drop = FALSE])
  }
  invisible(TRUE)
}
# Re-hydrate OPERATIONAL confirmations from the durable API (GET /status) on startup.
# Without this, a Connect Cloud restart wipes the Tracker's local confirmation state
# while the API (and the Scenario tool) still show the station operational — a split
# brain where the Reset button isn't even available to fix it. Re-hydrating keeps the
# Tracker's view in sync with the durable source of truth. Runs after init_db().
sync_operational_from_api <- function() {
  if (!nzchar(STATUS_API_URL)) return(invisible(FALSE))
  ops <- tryCatch({
    resp <- request(paste0(STATUS_API_URL, "/status")) |> req_timeout(15) |> req_perform()
    resp_body_json(resp, simplifyVector = FALSE)$stations
  }, error = function(e) { message("Operational re-hydrate failed: ", conditionMessage(e)); NULL })
  if (is.null(ops) || length(ops) == 0) return(invisible(FALSE))
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  for (s in ops) {
    key <- s$station_id %||% ""
    if (!nzchar(key)) next
    dbExecute(con, "UPDATE tracking SET ps_status='Operational (confirmed)', operational='Yes',
                    review_flag='', review_at='' WHERE station_id = ?", params = list(key))
  }
  invisible(TRUE)
}

# ---- base data from the TEVI master CSV -------------------------------------
# Reads master_Data_TN.csv (unified TEVI export). The station TYPE is carried in the
# data_source column (Coming_Soon / Open_Creditable / NEVI Awarded Sites / Other_DCFC),
# confidence in the `confidence` column, and the PlugShare link in `PlugShare_link` —
# so no verifications.R is needed. ps_status/operational are derived from the type.
build_base <- function() {
  nzc <- function(x) { x <- as.character(x); ifelse(is.na(x), "", x) }
  raw <- read.csv(find_file("master_Data_TN.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  master_df <- do.call(rbind, lapply(seq_len(nrow(raw)), function(i) {
    r   <- raw[i, ]
    ds  <- nzc(r$data_source)
    url <- nzc(r$PlugShare_link)
    loc <- if (grepl("location/[0-9]+", url)) sub(".*location/([0-9]+).*", "\\1", url) else ""
    op  <- ds %in% c("Open_Creditable", "Other_DCFC")            # already open/operational
    ps  <- switch(ds, "Coming_Soon" = "Coming Soon", "Open_Creditable" = "Operational",
                  "NEVI Awarded Sites" = "Awarded", "Other_DCFC" = "Existing", "Coming Soon")
    # "Ports" = total connectors (CCS1 + NACS). This is what the tracker's Ports column and the
    # popup show — NOT charger_count (a site can have 2 chargers but 4 ports). Awarded sites have
    # no published port detail yet, so show the NEVI minimum of 4; fall back to charger_count.
    ccs  <- suppressWarnings(as.numeric(r$ccs1_ports))
    nacs <- suppressWarnings(as.numeric(r$nacs_ports))
    tot  <- sum(c(ccs, nacs), na.rm = TRUE)
    ports_val <- if (ds == "NEVI Awarded Sites") "4" else if (tot > 0) as.character(tot) else nzc(r$charger_count)
    data.frame(
      station_id = slugify(nzc(r$address)), station_name = nzc(r$station_name),
      address = nzc(r$address), state = nzc(r$state),
      lat = suppressWarnings(as.numeric(r$latitude)),
      lon = suppressWarnings(as.numeric(r$longitude)),
      location_id = loc, plugshare_url = url,
      ps_status = ps, operational = if (op) "Yes" else "",
      master_operational = op,
      network = nzc(r$network),
      plugs = nzc(r$ccs1_ports), chargers = ports_val, notes = "",  # `chargers` field holds the PORT count (shown as "Ports")
      open_date = "", verified = if (op) "Yes" else "",
      data_source = ds, confidence_level = nzc(r$confidence), is_custom = FALSE,
      stringsAsFactors = FALSE)
  }))
  master_df <- master_df[!duplicated(master_df$station_id), , drop = FALSE]  # de-dupe by id
  # Fold in any user-added (custom) stations so they are first-class members.
  cu <- custom_to_base(read_custom_stations())
  if (!is.null(cu)) {
    cu <- cu[!cu$station_id %in% master_df$station_id, , drop = FALSE]   # de-dupe by id
    if (nrow(cu) > 0) master_df <- rbind(master_df, cu[, names(master_df)])
  }
  master_df
}
sync_custom_from_api()   # re-hydrate local custom stations from the durable API first
BASE <- build_base()

# ---- SQLite store -----------------------------------------------------------
EDIT_FIELDS <- c("ps_status","operational","network","plugs","chargers","notes","verified")
init_db <- function(base) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "tracking")) {
    seed <- base[, c("station_id", EDIT_FIELDS)]
    seed$verified_date <- ifelse(seed$verified == "Yes", "2026-06-05", "")
    seed$updated_at <- ""; seed$last_checked <- ""
    seed$review_flag <- ""; seed$review_at <- ""   # "" = none, "candidate" = needs human review
    dbWriteTable(con, "tracking", seed)
  } else {
    flds <- dbListFields(con, "tracking")
    if (!"last_checked" %in% flds)
      dbExecute(con, "ALTER TABLE tracking ADD COLUMN last_checked TEXT DEFAULT ''")
    if (!"review_flag" %in% flds)
      dbExecute(con, "ALTER TABLE tracking ADD COLUMN review_flag TEXT DEFAULT ''")
    if (!"review_at" %in% flds)
      dbExecute(con, "ALTER TABLE tracking ADD COLUMN review_at TEXT DEFAULT ''")
    have <- dbGetQuery(con, "SELECT station_id FROM tracking")$station_id
    new <- base[!base$station_id %in% have, ]
    if (nrow(new) > 0) {
      add <- new[, c("station_id", EDIT_FIELDS)]
      add$verified_date <- ifelse(add$verified == "Yes", "2026-06-05", "")
      add$updated_at <- ""; add$last_checked <- ""
      add$review_flag <- ""; add$review_at <- ""
      dbAppendTable(con, "tracking", add)
    }
  }
}
init_db(BASE)
sync_operational_from_api()   # re-hydrate confirmations so the Tracker matches the API
read_tracking <- function() {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con)); dbReadTable(con, "tracking")
}
update_cell <- function(id, field, value) {
  if (!field %in% EDIT_FIELDS) return(invisible())
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, sprintf("UPDATE tracking SET %s = ?, updated_at = ? WHERE station_id = ?", field),
            params = list(value, as.character(Sys.time()), id))
}
update_status <- function(id, ps_status, operational) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE tracking SET ps_status=?, operational=?, last_checked=?, updated_at=? WHERE station_id=?",
            params = list(ps_status, operational, as.character(Sys.time()),
                          as.character(Sys.time()), id))
}
touch_checked <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE tracking SET last_checked=? WHERE station_id=?",
            params = list(as.character(Sys.time()), id))
}
# Raise a review flag (a candidate found by the checker) — does NOT change status.
flag_review <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE tracking SET review_flag='candidate', review_at=?, last_checked=? WHERE station_id=?",
            params = list(as.character(Sys.time()), as.character(Sys.time()), id))
}
# Clear a review flag without changing status (a "Dismiss" / "still coming soon").
clear_review <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE tracking SET review_flag='', review_at='', updated_at=? WHERE station_id=?",
            params = list(as.character(Sys.time()), id))
}
# A human confirms the station is live: set Operational AND clear the flag.
confirm_operational <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE tracking SET ps_status='Operational (confirmed)', operational='Yes',
                  review_flag='', review_at='', last_checked=?, updated_at=? WHERE station_id=?",
            params = list(as.character(Sys.time()), as.character(Sys.time()), id))
}
# Undo a confirmation: put the station back to Coming Soon AND re-raise the review
# flag, so it returns to the "needs review" queue exactly as before it was confirmed.
# Used by the "Reset to Coming Soon" button for repeatable testing / demos.
reset_review <- function(id) {
  con <- dbConnect(SQLite(), DB_PATH); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE tracking SET ps_status='Coming Soon', operational='',
                  review_flag='candidate', review_at=?, last_checked=?, updated_at=? WHERE station_id=?",
            params = list(as.character(Sys.time()), as.character(Sys.time()),
                          as.character(Sys.time()), id))
}

# ---- ArcGIS Online sync (STAGED — inert until switched on) -------------------
# Publishes the tracker's authoritative Coming Soon / Open creditable stations to
# the two TDOT hosted feature layers so the public web map stays current.
# COMPLETELY INERT until ALL of these are true (nothing runs otherwise):
#   1. env  ARC_SYNC_ENABLED=true   (the master switch — default off)
#   2. credentials with EDIT rights on the layer — EITHER (simplest)
#         env  ARC_API_KEY            (an ArcGIS API key scoped to edit the layer)
#      OR an OAuth app credential
#         env  ARC_CLIENT_ID / ARC_CLIENT_SECRET  (optional ARC_PORTAL,
#              default https://hntbcorp.maps.arcgis.com)
#   3. editing enabled on the layers + install.packages(c("arcgislayers","sf"))
# See the field-guide "Build sheet: TN ArcGIS wiring" for the security/view model.
# The tracker WRITES to an org-only EDITABLE VIEW — never the public source — so the
# public source layer (read by the web map) stays read-only and safe from anonymous
# edits. Override with ARC_SVC_URL if the view is ever recreated at a new URL.
ARC_SVC <- Sys.getenv("ARC_SVC_URL", paste0(
  "https://services.arcgis.com/rD2ylXRs80UroD90/arcgis/rest/services/",
  "TEVI_Creditable_-_Tracker_(editable)/FeatureServer"))
ARC_LAYER <- list(coming_soon = paste0(ARC_SVC, "/0"),   # String port fields
                  open        = paste0(ARC_SVC, "/1"))   # Integer port fields

sync_to_arcgis <- function() {
  if (!identical(tolower(Sys.getenv("ARC_SYNC_ENABLED", "")), "true"))
    return(invisible(FALSE))                              # <-- master switch: OFF by default
  if (!requireNamespace("arcgislayers", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    warning("ArcGIS sync: install.packages(c('arcgislayers','sf')) first")
    return(invisible(FALSE))
  }
  # 1) current truth = master CSV + the tracker's confirmed-operational overrides
  raw <- read.csv(find_file("master_Data_TN.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  con <- dbConnect(SQLite(), DB_PATH)
  trk <- dbGetQuery(con, "SELECT station_id, operational FROM tracking"); dbDisconnect(con)
  live <- trk$station_id[trk$operational == "Yes"]
  flip <- raw$data_source == "Coming_Soon" & vapply(raw$address, slugify, "") %in% live
  raw$data_source[flip] <- "Open_Creditable"; raw$status[flip] <- "Open"   # confirmed CS -> Open

  # 2) build sf points in the layer's spatial reference (2274), typed per layer
  build <- function(df, integer_ports) {
    if (nrow(df) == 0) return(NULL)
    portcols <- c("ccs1_ports","ccs1_power_per_port","nacs_ports","nacs_power_per_port","charger_count")
    for (col in intersect(portcols, names(df)))
      df[[col]] <- if (integer_ports) suppressWarnings(as.integer(df[[col]])) else as.character(df[[col]])
    keep <- c("station_name","site_type","status","address","state","latitude","longitude",
              "network","PlugShare_link", portcols, "open_24_7","NEVI_creditable","AFC")
    df <- df[, intersect(keep, names(df)), drop = FALSE]
    names(df)[names(df) == "latitude"] <- "lat"; names(df)[names(df) == "longitude"] <- "long"
    sf::st_transform(sf::st_as_sf(df, coords = c("long","lat"), crs = 4326, remove = FALSE), 2274)
  }
  cs   <- build(raw[raw$data_source == "Coming_Soon", ],     FALSE)   # Layer 0
  open <- build(raw[raw$data_source == "Open_Creditable", ], TRUE)    # Layer 1

  # 3) authenticate + replace each layer's features. Prefer an API key (simplest —
  #    scope "edit features" to the layer); fall back to an OAuth app credential.
  tok <- if (nzchar(Sys.getenv("ARC_API_KEY"))) {
    arcgisutils::auth_key(Sys.getenv("ARC_API_KEY"))
  } else {
    arcgisutils::auth_client(
      client = Sys.getenv("ARC_CLIENT_ID"),      # arcgisutils uses `client`/`secret`
      secret = Sys.getenv("ARC_CLIENT_SECRET"),
      host   = Sys.getenv("ARC_PORTAL", "https://hntbcorp.maps.arcgis.com"))
  }
  arcgisutils::set_arc_token(tok)
  push <- function(url, sfx) {
    if (is.null(sfx)) return(invisible())
    lyr <- arcgislayers::arc_open(url)
    arcgislayers::truncate_layer(lyr); arcgislayers::add_features(lyr, sfx)
  }
  push(ARC_LAYER$coming_soon, cs); push(ARC_LAYER$open, open)
  invisible(TRUE)
}

# ---- Shared Status API integration (the "post" side) ------------------------
# When a station is confirmed operational here, ALSO push it to the shared status
# API so the Scenario Analysis tool can read it on refresh and move the station
# into its Post-March 2026 layer. Set these as env vars (never hard-code):
#   STATUS_API_URL / STATUS_API_TOKEN  (now defined near the top of this file).

# Push one confirmed-operational station to the API. Safe no-op if the URL is not
# configured, and wrapped in tryCatch so an API hiccup never breaks the Tracker.
post_operational <- function(station_id, address, open_date = "") {
  if (!nzchar(STATUS_API_URL)) return(invisible(FALSE))   # not configured -> skip
  tryCatch({
    request(paste0(STATUS_API_URL, "/confirm")) |>        # build the endpoint URL
      req_method("POST") |>                               # we are WRITING
      req_headers(`X-API-Token` = STATUS_API_TOKEN) |>    # prove we're allowed
      req_body_json(list(                                 # the JSON payload
        station_id = station_id, address = address,
        open_date = open_date, confirmed_by = "tracker")) |>
      req_timeout(15) |>
      req_perform()                                       # send it
    TRUE
  }, error = function(e) {
    message("Status API push failed: ", conditionMessage(e)); FALSE
  })
}

# Remove one station from the shared API (the mirror of post_operational). Called
# by "Reset to Coming Soon" so the Scenario tool drops the station on its next
# refresh. Safe no-op if the URL is not configured; never breaks the Tracker.
delete_operational <- function(station_id) {
  if (!nzchar(STATUS_API_URL)) return(invisible(FALSE))   # not configured -> skip
  tryCatch({
    request(paste0(STATUS_API_URL, "/confirm")) |>        # same endpoint as POST
      req_method("DELETE") |>                             # but we are REMOVING
      req_headers(`X-API-Token` = STATUS_API_TOKEN) |>    # prove we're allowed
      req_body_json(list(station_id = station_id)) |>     # which station to drop
      req_timeout(15) |>
      req_perform()
    TRUE
  }, error = function(e) {
    message("Status API delete failed: ", conditionMessage(e)); FALSE
  })
}

# ---- PlugShare status reader ------------------------------------------------
# The authoritative fields (coming_soon / "status":"OPERATIONAL") live behind
# PlugShare's authenticated JSON API (401 without a paid key), so they are NOT in
# the public HTML. We therefore read the public page's og:title and classify
# CONSERVATIVELY into three outcomes:
#   "coming_soon"  - title clearly still says "(Coming Soon)"          -> no change
#   "candidate"    - a SPECIFIC station title with NO "Coming Soon"     -> FLAG for human review
#   "inconclusive" - generic/blank title or fetch failed               -> ignore (this is what
#                    caused the Wawa false-positive: PlugShare sometimes serves the generic
#                    homepage title, which the old code misread as "operational")
# A candidate is NEVER auto-promoted; a person confirms it on PlugShare first.
PS_GENERIC_TITLE <- "(?i)find a place to charge|ev charging station map|^plugshare\\s*$|^plugshare\\s*-\\s*find"

scrape_plugshare_status <- function(id) {
  if (is.na(id) || id == "") return(list(ok = FALSE, outcome = "inconclusive"))
  url <- paste0("https://www.plugshare.com/location/", id)
  res <- tryCatch(
    httr2::req_perform(httr2::req_timeout(
      httr2::req_user_agent(httr2::request(url),
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"), 25)),
    error = function(e) NULL)
  if (is.null(res) || httr2::resp_status(res) != 200)
    return(list(ok = FALSE, outcome = "inconclusive", title = ""))
  html <- httr2::resp_body_string(res)
  m <- regmatches(html, regexpr('property="og:title"[^>]*content="[^"]*"', html))
  title <- if (length(m)) sub('.*content="([^"]*)".*', "\\1", m) else ""
  title <- trimws(title)
  is_generic <- !nzchar(title) || grepl(PS_GENERIC_TITLE, title, perl = TRUE)
  is_cs      <- grepl("coming soon", title, ignore.case = TRUE)
  # A trustworthy "operational" signal = a specific location title
  # (PlugShare uses "Name | City, ST | EV Station") with no "Coming Soon".
  is_specific_open <- !is_generic && !is_cs && grepl("\\| EV Station", title, ignore.case = TRUE)
  outcome <- if (is_cs) "coming_soon"
             else if (is_specific_open) "candidate"
             else "inconclusive"
  list(ok = TRUE, outcome = outcome, title = title)
}

# ---- status display helpers + popup -----------------------------------------
# Treat a logical vector's NAs as FALSE (master_operational can be NA after a join).
isTRUE_vec <- function(x) !is.na(x) & x
status_class <- function(op, ps) ifelse(grepl("Existing", ps), "dcfc",
  ifelse(grepl("Awarded", ps), "awarded",
  ifelse(op == "Yes", "op",
  ifelse(grepl("Repair", ps), "ur", ifelse(grepl("Not found", ps), "nf", "cs")))))
status_color <- function(cls) c(op = INDOT$green, cs = INDOT$teal, nf = INDOT$brown,
                                ur = INDOT$red, rv = INDOT$amber, nc = INDOT$newcs,
                                awarded = INDOT$navy, dcfc = "#3a3a3a")[cls]  # small dark context dots
status_label <- function(op, ps) ifelse(grepl("Existing", ps), "Existing DCFC",
  ifelse(grepl("Awarded", ps), "Awarded",
  ifelse(op == "Yes", "Operational",
  ifelse(ps == "", "Coming Soon", ps))))

# ---- Station-type & confidence labels/colors (shared with the Scenario tool) --
# Layer/type labels — kept consistent with the TDOT ArcGIS web map legend.
TYPE_LABELS <- c("Coming_Soon" = "Creditable Stations (Coming Soon)",
                 "NEVI Awarded Sites" = "TEVI Round 1 Award Stations",
                 "Open_Creditable" = "Creditable Stations (Open)", "Other_DCFC" = "Existing DCFC")
type_label <- function(ds) {
  ds <- as.character(ds); lbl <- unname(TYPE_LABELS[ds])
  ifelse(is.na(lbl), ifelse(nzchar(ds), ds, "Coming Soon"), lbl)
}
# Tracker confidence palette: High = Indiana University crimson (#990000), Medium =
# yellow, Low = gray. Purple is NOT a confidence color — it belongs only to the
# "New Coming-Soon" value box (the added-stations category). Independent of the Scenario tool.
CONF_COLORS <- c("High" = "#990000", "Medium" = "#ffc107", "Low" = "#6c757d")
conf_color <- function(cl) { v <- unname(CONF_COLORS[as.character(cl)]); ifelse(is.na(v), "#6c757d", v) }
# The one-word meaning shown alongside each confidence level, per manager request:
# High = Constructed, Medium = Plans Exist, Low = Announced.
CONF_MEANING <- c("High" = "Constructed", "Medium" = "Plans Exist", "Low" = "Announced")
conf_label   <- function(cl) { m <- CONF_MEANING[as.character(cl)]
  ifelse(is.na(m), as.character(cl), sprintf("%s Confidence (%s)", cl, m)) }
# Confidence choices: the option LABEL carries the definition (shown in the dropdown),
# while the stored VALUE stays High/Medium/Low for the rest of the app.
CONF_CHOICES <- c("High Confidence (Constructed)"   = "High",
                  "Medium Confidence (Plans Exist)" = "Medium",
                  "Low Confidence (Announced)"      = "Low")

# ---- Tennessee boundary -----------------------------------------------------
# Prefer a TDOT/TN state-boundary shapefile (drop it in a "Tennessee_State_Boundary"
# folder) via {sf}, transformed to WGS84. If it's absent, fall back to the {maps}
# Tennessee outline automatically — so the app works with or without the shapefile.
IN_SHP_NAME <- file.path("Tennessee_State_Boundary", "Tennessee_State_Boundary.shp")
IN_SHP_PATH <- {
  cands <- c(IN_SHP_NAME, file.path("data", IN_SHP_NAME), file.path("..", IN_SHP_NAME))
  hit <- cands[file.exists(cands)]
  if (length(hit)) hit[1] else NA_character_
}
IN_POLY <- NULL
if (!is.na(IN_SHP_PATH) && requireNamespace("sf", quietly = TRUE)) {
  IN_POLY <- tryCatch({
    g <- sf::st_transform(sf::st_read(IN_SHP_PATH, quiet = TRUE), 4326)
    sf::st_zm(sf::st_make_valid(g))
  }, error = function(e) NULL)
}
# Always keep a lat/lon outline available (used for fallback + neighbor states).
IN_BORDER <- map("state", "tennessee", plot = FALSE, fill = FALSE)
BS_BORDER <- map("state", c("kentucky","virginia","north carolina","georgia",
                            "alabama","mississippi","arkansas","missouri"),
                 plot = FALSE, fill = FALSE)
# Indiana bounding box for "zoom to whole state" (from the shapefile if we have it).
IN_BBOX <- if (!is.null(IN_POLY)) {
  as.list(sf::st_bbox(IN_POLY))
} else {
  list(xmin = min(IN_BORDER$x, na.rm = TRUE), xmax = max(IN_BORDER$x, na.rm = TRUE),
       ymin = min(IN_BORDER$y, na.rm = TRUE), ymax = max(IN_BORDER$y, na.rm = TRUE))
}

# Add the Indiana boundary to a leaflet map: filled polygon if we have the shapefile,
# otherwise a polyline from {maps}. `fill` controls the subtle navy tint.
add_indiana <- function(lf, fill = TRUE) {
  if (!is.null(IN_POLY)) {
    lf %>% leaflet::addPolygons(data = IN_POLY, color = "#000000", weight = 2.5,
      opacity = .95, fill = fill, fillColor = "#000000", fillOpacity = if (fill) 0.04 else 0,
      smoothFactor = 0.5)
  } else {
    lf %>% addPolylines(lng = IN_BORDER$x, lat = IN_BORDER$y, color = "#000000",
      weight = 2.5, opacity = .95)
  }
}

# ---- Alternative Fuel Corridors (FHWA) — TN context layer -------------------
# Pulled live from the federal FHWA ArcGIS service, filtered to STATE = 'TN'.
# Fetched once at startup; a network hiccup just means no AFC layer (app still works).
AFC_URL <- paste0("https://services.arcgis.com/rD2ylXRs80UroD90/arcgis/rest/services/",
                  "AltFuelCorridors_R1to7_WGS84_Public_View/FeatureServer/0/query")
AFC_TN <- tryCatch({
  if (!requireNamespace("sf", quietly = TRUE)) NULL else {
    resp <- request(AFC_URL) |>
      req_url_query(where = "STATE='TN'", outFields = "PRIMARY_NA,ROADTYPE",
                    returnGeometry = "true", f = "geojson", outSR = "4326") |>
      req_timeout(25) |> req_perform()
    g <- sf::st_read(resp_body_string(resp), quiet = TRUE)
    if (nrow(g) == 0) NULL else sf::st_zm(g)
  }
}, error = function(e) { message("AFC fetch failed: ", conditionMessage(e)); NULL })

# ---- Inline SVG icon set (enterprise popup cards) ---------------------------
# 16px line icons, currentColor-driven so we can tint per row.
svg_icon <- function(name, col = "#5b6472", sz = 15) {
  paths <- list(
    pin   = '<path d="M8 1.5c-2.5 0-4.5 2-4.5 4.5 0 3.2 4.5 8 4.5 8s4.5-4.8 4.5-8C12.5 3.5 10.5 1.5 8 1.5z"/><circle cx="8" cy="6" r="1.8" fill="#fff"/>',
    plug  = '<path d="M5 1.5v3M11 1.5v3M3.5 4.5h9v2.5a4.5 4.5 0 0 1-9 0V4.5zM8 11.5v3"/>',
    network = '<circle cx="8" cy="3.2" r="1.7"/><circle cx="3.4" cy="12" r="1.7"/><circle cx="12.6" cy="12" r="1.7"/><path d="M8 4.9 4.2 10.6M8 4.9l3.8 5.7M5 12h6"/>',
    id    = '<rect x="2" y="3.5" width="12" height="9" rx="1.5"/><path d="M4.5 6.5h3M4.5 9h5"/>',
    check = '<circle cx="8" cy="8" r="6.3"/><path d="M5.3 8.2l1.9 1.9 3.5-3.8" stroke="#fff"/>',
    note  = '<path d="M3.5 2.5h9v11l-2-1.4-2 1.4-2-1.4-2 1.4v-11z"/><path d="M5.5 5.5h5M5.5 8h5"/>',
    ext   = '<path d="M6 3.5H3.5v9h9V10M9.5 3.5H13v3.5M13 3.5 7.5 9"/>',
    clock = '<circle cx="8" cy="8" r="6.3"/><path d="M8 4.6V8l2.4 1.4"/>',
    warn  = '<path d="M8 2.3 1.9 12.8h12.2L8 2.3z"/><path d="M8 6.4v3M8 11.1v.05"/>'
  )
  sprintf('<svg viewBox="0 0 16 16" width="%d" height="%d" fill="none" stroke="%s" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round" style="vertical-align:-2px;flex:none">%s</svg>',
          sz, sz, col, paths[[name]])
}

# SVG as an htmltools tag (for use in UI, e.g. the Review tab), not a popup string.
svg_to_tag <- function(name, col = "#5b6472", sz = 16) HTML(svg_icon(name, col, sz))

make_popup <- function(r) {
  esc  <- function(v) htmlEscape(ifelse(is.na(v), "", as.character(v)))
  has  <- function(v) !is.na(v) && nzchar(as.character(v))
  navy <- INDOT$navy
  row  <- function(icon, k, v) if (has(v)) sprintf(
    "<div style='display:flex;align-items:flex-start;gap:8px;padding:5px 0;border-top:1px solid #eef1f6'>
       %s<div style='flex:1'><div style='font-size:10px;text-transform:uppercase;letter-spacing:.4px;color:#9aa3b2;font-weight:700'>%s</div>
       <div style='font-size:12.5px;color:#1d2633;font-weight:600'>%s</div></div></div>",
    svg_icon(icon, navy), k, esc(v)) else ""
  plugchips <- if (has(r$plugs))
    paste0("<div style='margin:8px 0 2px'>",
      paste(vapply(trimws(strsplit(as.character(r$plugs), ",")[[1]]), function(p)
        sprintf("<span style='display:inline-block;background:#eef2f8;color:#33415c;border-radius:6px;padding:2px 9px;font-size:11px;font-weight:700;margin:2px 4px 2px 0'>%s</span>", esc(p)),
        character(1)), collapse=""), "</div>") else ""
  cta <- if (has(r$plugshare_url))
    sprintf("<a href='%s' target='_blank' rel='noopener' style='display:flex;align-items:center;justify-content:center;gap:7px;margin-top:11px;background:%s;color:#fff;text-decoration:none;font-weight:700;padding:9px 12px;border-radius:9px;font-size:12.5px'>%s Open full PlugShare listing</a>",
            r$plugshare_url, navy, svg_icon("ext", "#ffffff"))
    else "<div style='margin-top:9px;color:#9aa3b2;font-size:11.5px;font-style:italic'>Not listed on PlugShare</div>"
  notes <- if (has(r$notes))
    sprintf("<div style='display:flex;gap:8px;margin-top:8px;background:#f7f9fc;border-radius:8px;padding:8px 10px'>%s<div style='font-size:11.5px;color:#566' >%s</div></div>",
            svg_icon("note", "#9aa3b2"), esc(r$notes)) else ""
  # Confidence pill for added stations (shown next to the status badge when present).
  conf_badge <- if (has(r$confidence_level))
    sprintf("<span style='display:inline-flex;align-items:center;gap:4px;background:%s;color:%s;padding:3px 10px;border-radius:999px;font-size:11px;font-weight:800;letter-spacing:.3px;margin-left:6px'>%s</span>",
            conf_color(r$confidence_level),
            if (identical(as.character(r$confidence_level), "Medium")) "#3a3000" else "#ffffff",
            esc(toupper(conf_label(r$confidence_level)))) else ""
  sprintf("<div style='font-family:Inter,Segoe UI,Arial;min-width:262px;max-width:312px'>
      <div style='display:flex;align-items:flex-start;gap:9px'>
        %s
        <div style='flex:1'>
          <div style='font-weight:800;color:%s;font-size:14.5px;line-height:1.2'>%s</div>
          <div style='color:#6b7280;font-size:11.5px;margin-top:2px'>%s</div>
        </div>
      </div>
      <div style='margin-top:9px'>
        <span style='display:inline-flex;align-items:center;gap:5px;background:%s;color:#fff;padding:3px 11px;border-radius:999px;font-size:11px;font-weight:800;letter-spacing:.3px'>%s%s</span>%s
      </div>
      %s
      <div style='margin-top:6px'>%s%s%s%s%s</div>
      %s%s
    </div>",
    svg_icon("pin", r$color, 22), navy, esc(r$station_name), esc(r$address),
    r$color, svg_icon("check", "#ffffff", 12), esc(toupper(r$disp)), conf_badge,
    plugchips,
    row("network", "Network", r$network), row("plug", "Ports", r$chargers),
    row("id", "PlugShare ID", r$location_id),
    row("check", "Open date", if (!is.null(r$open_date)) r$open_date else ""),
    row("check", "Verified", r$verified_date),
    notes, cta)
}

# Enterprise popup for the Review-tab map (flagged "needs review" stations).
# Same design language as make_popup, tuned for the review context (amber pin,
# a "NEEDS REVIEW" status pill, and a Verify-on-PlugShare call to action).
make_review_popup <- function(r) {
  esc  <- function(v) htmlEscape(ifelse(is.na(v), "", as.character(v)))
  has  <- function(v) !is.na(v) && nzchar(as.character(v))
  navy <- INDOT$navy; amber <- INDOT$amber
  row  <- function(icon, k, v) if (has(v)) sprintf(
    "<div style='display:flex;align-items:flex-start;gap:8px;padding:5px 0;border-top:1px solid #eef1f6'>
       %s<div style='flex:1'><div style='font-size:10px;text-transform:uppercase;letter-spacing:.4px;color:#9aa3b2;font-weight:700'>%s</div>
       <div style='font-size:12.5px;color:#1d2633;font-weight:600'>%s</div></div></div>",
    svg_icon(icon, navy), k, esc(v)) else ""
  cta <- if (has(r$plugshare_url))
    sprintf("<a href='%s' target='_blank' rel='noopener' style='display:flex;align-items:center;justify-content:center;gap:7px;margin-top:11px;background:%s;color:#fff;text-decoration:none;font-weight:700;padding:9px 12px;border-radius:9px;font-size:12.5px'>%s Verify on PlugShare</a>",
            r$plugshare_url, navy, svg_icon("ext", "#ffffff"))
    else "<div style='margin-top:9px;color:#9aa3b2;font-size:11.5px;font-style:italic'>Not listed on PlugShare</div>"
  flagged <- if (has(r$review_at)) substr(as.character(r$review_at), 1, 16) else ""
  sprintf("<div style='font-family:Inter,Segoe UI,Arial;min-width:248px;max-width:302px'>
      <div style='display:flex;align-items:flex-start;gap:9px'>
        %s
        <div style='flex:1'>
          <div style='font-weight:800;color:%s;font-size:14.5px;line-height:1.2'>%s</div>
          <div style='color:#6b7280;font-size:11.5px;margin-top:2px'>%s</div>
        </div>
      </div>
      <div style='margin-top:9px'>
        <span style='display:inline-flex;align-items:center;gap:5px;background:#fff6e5;color:#8a5d00;border:1px solid #f0d9a8;padding:3px 11px;border-radius:999px;font-size:11px;font-weight:800;letter-spacing:.3px'>%s NEEDS REVIEW</span>
      </div>
      <div style='margin-top:6px'>%s%s%s</div>
      %s
    </div>",
    svg_icon("pin", amber, 22), navy, esc(r$station_name), esc(r$address),
    svg_icon("warn", "#8a5d00", 12),
    row("network", "Network", r$network),
    row("id", "PlugShare ID", r$location_id),
    row("clock", "Flagged", flagged),
    cta)
}

# =============================================================================
# UI
# =============================================================================
theme_app <- bs_theme(version = 5, bootswatch = "flatly",
  primary = INDOT$navy, secondary = INDOT$gold, success = INDOT$green,
  info = INDOT$teal, danger = INDOT$red,
  base_font = font_google("Inter"), heading_font = font_google("Inter"), font_scale = 0.95)

kpi_row <- function() layout_columns(col_widths = NULL, fill = FALSE,
  value_box("Tracked", textOutput("kpi_total", inline = TRUE),
            showcase = bs_icon("ev-station-fill"), theme = value_box_theme(bg = INDOT$navy, fg = "white")),
  value_box("Creditable · Open", textOutput("kpi_op", inline = TRUE),
            showcase = bs_icon("check-circle-fill"), theme = value_box_theme(bg = INDOT$green, fg = "white")),
  value_box("Creditable · Coming Soon", textOutput("kpi_cs", inline = TRUE),
            showcase = bs_icon("clock-fill"), theme = value_box_theme(bg = INDOT$teal, fg = "white")),
  value_box("New Coming-Soon", textOutput("kpi_nc", inline = TRUE),
            showcase = bs_icon("plus-circle-fill"), theme = value_box_theme(bg = INDOT$newcs, fg = "white")),
  value_box("Needs Review", textOutput("kpi_review", inline = TRUE),
            showcase = bs_icon("exclamation-triangle-fill"), theme = value_box_theme(bg = INDOT$amber, fg = "white")),
  value_box("Award Stations", textOutput("kpi_awarded", inline = TRUE),
            showcase = bs_icon("award-fill"), theme = value_box_theme(bg = INDOT$navy, fg = "white")),
  value_box("Existing DCFC", textOutput("kpi_dcfc", inline = TRUE),
            showcase = bs_icon("ev-station"), theme = value_box_theme(bg = INDOT$gray, fg = "white")))

# Manual "Sync to ArcGIS map" control — only appears once sync is switched on
# (ARC_SYNC_ENABLED=true), so it's invisible until the integration is live.
arc_sync_ui <- if (identical(tolower(Sys.getenv("ARC_SYNC_ENABLED", "")), "true")) {
  tagList(
    tags$hr(),
    tags$label("ArcGIS web map", class = "filter-label"),
    actionButton("sync_arcgis", "Sync to ArcGIS map", icon = icon("cloud-arrow-up"),
                 class = "btn btn-outline-primary btn-sm w-100"),
    tags$p(class = "text-muted small mt-2",
      "Re-publishes all creditable stations to the TDOT ArcGIS web map."))
} else NULL

app_sidebar <- sidebar(width = 300, title = "Filters & actions", open = "open",
  div(class = "filter-section",
    tags$label("Status", class = "filter-label"),
    checkboxGroupInput("flt_status", NULL,
      # Labels match the ArcGIS web-map legend; values stay the tracker's internal status keys.
      choices  = c("Creditable Stations (Open)"        = "Operational",
                   "Creditable Stations (Coming Soon)" = "Coming Soon",
                   "TEVI Round 1 Award Stations"       = "Awarded",
                   "Existing DCFC"                     = "Existing DCFC",
                   "Under Repair"                      = "Under Repair",
                   "Not found"                         = "Not found"),
      # "Existing DCFC" is context — OFF by default; toggle it on to see existing chargers.
      selected = c("Operational","Coming Soon","Awarded","Under Repair","Not found"))),
  div(class = "filter-section",
    tags$label("State", class = "filter-label"),
    checkboxGroupInput("flt_state", NULL,
      choices = sort(unique(BASE$state)), selected = sort(unique(BASE$state)))),
  arc_sync_ui,
  tags$hr(),
  tags$label("Live PlugShare check", class = "filter-label"),
  actionButton("check_all", "Check all stations now", icon = icon("satellite-dish"),
               class = "btn btn-gold btn-sm w-100"),
  tags$p(class = "text-muted small mt-2",
    "Scrapes each station's PlugShare page. Any station that no longer says “Coming Soon” ",
    "is ", tags$b("flagged for review"), " (see the Review tab) — nothing is changed automatically."),
  actionButton("refresh", "Reload saved data", icon = icon("rotate"),
               class = "btn btn-outline-primary btn-sm w-100 mt-1"),
  tags$p(class = "text-muted small mt-2", textOutput("last_check_txt")),
  div(class = "sidebar-logos",
    tags$img(src = "TDOT_logo.png", alt = "Tennessee Department of Transportation"),
    tags$span(class = "logo-chip", tags$img(src = "hntb_logo.png", alt = "HNTB"))))

main_ui <- page_navbar(
  title = tags$span(bs_icon("ev-station-fill", class = "me-2", style = "color:#fff;"),
                    "TEVI Tracker"),
  window_title = "TEVI Tracker — Tennessee NEVI",  # plain-text browser tab title (not the SVG)
  theme = theme_app, fillable = TRUE,
  navbar_options = navbar_options(bg = INDOT$gold, theme = "dark"),   # TDOT red top bar
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  sidebar = app_sidebar,

  nav_panel(tagList(bs_icon("geo-alt-fill"), " Map"),
    kpi_row(),
    card(full_screen = TRUE,
      card_header("Tennessee EV Stations — click a marker for PlugShare detail"),
      withSpinner(leafletOutput("map", height = "calc(100vh - 330px)"), color = INDOT$navy, type = 8))),

  nav_panel(tagList(bs_icon("table"), " Tracker"),
    card(full_screen = TRUE,
      card_header(tags$span("Editable station log — saved to SQLite, shared across users"),
        tags$span(class = "ms-auto", downloadButton("dl_csv", "Export CSV", class = "btn btn-sm btn-outline-primary"))),
      div(class = "p-2",
        div(class = "tbl-layer-bar",
          tags$span(class = "fw-bold small", "Layer:"),
          selectInput("tbl_layer", NULL, width = "230px",
            choices = c("All layers" = "all",
                        "Creditable Stations (Open)" = "Open_Creditable",
                        "Creditable Stations (Coming Soon)" = "Coming_Soon",
                        "TEVI Round 1 Award Stations" = "NEVI Awarded Sites",
                        "Existing DCFC" = "Other_DCFC"), selected = "all")),
        helpText("Double-click PlugShare status / Operational / Network / Ports / Notes to edit."),
        withSpinner(DTOutput("tbl"), color = INDOT$navy, type = 8)))),

  nav_panel(tagList(bs_icon("exclamation-triangle-fill"), " Review"),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(
          tags$span(tags$b("Review queue"), " — verify, then confirm"),
          tags$span(class = "ms-auto",
            actionButton("check_all2", "Run live check", icon = icon("satellite-dish"),
                         class = "btn btn-sm btn-gold"))),
        card_body(
          div(class = "review-intro",
            svg_to_tag("note", INDOT$navy, 16),
            tags$span("A station lands here when PlugShare stops listing it as “Coming Soon” ",
              "(or when you review an awarded site). Nothing changes automatically — verify it is ",
              "genuinely live on PlugShare, then click ", tags$b("Confirm operational"),
              " to mark it open.")),
          uiOutput("review_ui"))),
      card(full_screen = TRUE,
        card_header(tagList(bs_icon("geo-alt-fill"), " Where the flagged stations are")),
        card_body(class = "p-0",
          withSpinner(leafletOutput("review_map", height = "calc(100vh - 250px)"),
                      color = INDOT$navy, type = 8))))),

  nav_panel(tagList(bs_icon("ev-front-fill"), " PlugShare"),
    layout_columns(col_widths = c(4, 8),
      div(class = "ps-side",
        tags$div(class = "filter-label mb-2", "Station detail"),
        uiOutput("ps_panel")),
      card(full_screen = TRUE,
        card_header("Click a station on the map for its PlugShare detail"),
        card_body(padding = 0,
          withSpinner(leafletOutput("ps_map", height = "calc(100vh - 180px)"),
                      color = INDOT$navy, type = 8))))),

  nav_panel(tagList(bs_icon("plus-circle-fill"), " Add Station"),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(tags$span(tags$b("Add a Station")),
          tags$span(class = "ms-auto muted", "Saved to the tracker database")),
        card_body(fillable = FALSE, class = "addform",
          div(class = "addform-intro",
            svg_to_tag("note", INDOT$navy, 16),
            tags$span("Enter the details from the station's PlugShare page. Fields marked ",
              tags$span(class = "req", "*"), " are required. Once saved, the station appears on ",
              "the map and is watched by the live check — when PlugShare shows it operational, ",
              "it is flagged for review automatically.")),
          tags$div(class = "form-section-label", "Identity"),
          layout_columns(col_widths = c(6, 6),
            textInput("ns_name", HTML('Station name <span class="req">*</span>'),
                      placeholder = "e.g. Pilot Travel Center"),
            textInput("ns_address", HTML('Street address <span class="req">*</span>'),
                      placeholder = "e.g. 123 Main St, Nashville, TN 37203")),
          layout_columns(col_widths = c(4, 4, 4),
            selectInput("ns_state", HTML('State <span class="req">*</span>'),
                        choices = c("TN","KY","VA","NC","GA","AL","MS","AR","MO")),
            numericInput("ns_lat", HTML('Latitude <span class="req">*</span>'),
                         value = NA, min = -90, max = 90, step = 0.000001),
            numericInput("ns_lon", HTML('Longitude <span class="req">*</span>'),
                         value = NA, min = -180, max = 180, step = 0.000001)),
          layout_columns(col_widths = c(6, 6),
            selectInput("ns_type", HTML('Station type <span class="req">*</span>'),
                        choices = c("Creditable Stations (Coming Soon)" = "Coming_Soon",
                                    "TEVI Round 1 Award Stations" = "NEVI Awarded Sites",
                                    "Creditable Stations (Open)" = "Open_Creditable",
                                    "Existing DCFC" = "Other_DCFC")),
            conditionalPanel("input.ns_type == 'Coming_Soon'",   # confidence only for Coming Soon
              selectInput("ns_conf", "Confidence level",
                          choices = CONF_CHOICES, selected = "Medium"))),
          tags$div(class = "form-section-label", "PlugShare & charging"),
          layout_columns(col_widths = c(6, 6),
            textInput("ns_psid", "PlugShare location ID", placeholder = "e.g. 1090403"),
            textInput("ns_network", "Network", placeholder = "e.g. GM Energy, EVgo, Tesla")),
          layout_columns(col_widths = c(4, 4, 4),
            textInput("ns_plugs", "Connectors", placeholder = "e.g. CCS, NACS"),
            textInput("ns_chargers", "Ports / chargers", placeholder = "e.g. 4"),
            textInput("ns_open", "Expected open date", placeholder = "e.g. 2026-09")),
          textAreaInput("ns_notes", "Notes", height = "80px",
                        placeholder = "Anything useful for the next reviewer…"),
          uiOutput("ns_msg"),
          div(class = "addform-actions",
            actionButton("ns_save", tagList(bs_icon("save"), " Save to tracker database"),
                         class = "btn btn-gold"),
            actionButton("ns_clear", "Clear form", class = "btn btn-outline-secondary")))),
      card(full_screen = TRUE,
        card_header(tagList(bs_icon("info-circle"), " Where to find these")),
        card_body(fillable = FALSE, class = "addhelp-body",
          tags$div(class = "addhelp",
            tags$p(tags$b("PlugShare location ID"), " — open the station on ",
              tags$a(href = "https://www.plugshare.com", target = "_blank", rel = "noopener",
                     "plugshare.com"), "; the number at the end of the URL ",
              tags$code(".../location/1090403"), " is the ID. Adding it lets the live check ",
              "watch the station automatically."),
            tags$p(tags$b("Latitude / longitude"), " — right-click the spot in Google Maps and ",
              "click the coordinates to copy them (e.g. ", tags$code("36.163, -86.781"), ")."),
            tags$p(tags$b("Network · connectors · ports"), " — read these off the PlugShare ",
              "listing (the network badge, the plug types, and the number of stalls).")),
          tags$hr(),
          tags$div(class = "form-section-label", "Recently added"),
          uiOutput("ns_recent"))))),

  nav_spacer(),
  nav_item(
    actionButton("logout_btn", tagList(bs_icon("box-arrow-right"), " Logout"),
                 class = "btn btn-sm logout-btn")),
  nav_panel(tagList(bs_icon("info-circle"), " About"),
    card(class = "about-card", card_body(class = "about-body",
      div(class = "about-hero",
        div(class = "about-hero-eyebrow", "TENNESSEE NEVI PROGRAM"),
        div(class = "about-hero-title", "Station Tracker & Data Hub"),
        div(class = "about-hero-sub",
          "Track Tennessee's NEVI charging stations against PlugShare - Coming Soon, Awarded, and ",
          "open creditable sites - so you know the moment each one goes live, in one shared, ",
          "always-current view.")),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("grid-1x2-fill"), tags$span("Getting around")),
        div(class = "about-grid",
          div(class = "about-card2", bs_icon("geo-alt-fill"),
              tags$div(tags$b("Map"), tags$p("Every tracked station, colored by status."))),
          div(class = "about-card2", bs_icon("table"),
              tags$div(tags$b("Tracker"), tags$p("Editable log - double-click a cell to edit."))),
          div(class = "about-card2", bs_icon("exclamation-triangle-fill"),
              tags$div(tags$b("Review"), tags$p("Verify flagged stations, then confirm them operational."))),
          div(class = "about-card2", bs_icon("ev-front-fill"),
              tags$div(tags$b("PlugShare"), tags$p("Per-station detail plus a link to the live listing."))),
          div(class = "about-card2", bs_icon("plus-circle-fill"),
              tags$div(tags$b("Add Station"), tags$p("Add a new station of any type."))))),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("diagram-3-fill"), tags$span("How tracking works")),
        tags$p(class = "about-lead", "Every tracked station moves through a simple lifecycle:"),
        div(class = "about-tier tier-a",
          div(class = "about-tier-tag", "Coming Soon / Awarded"),
          div(class = "about-tier-body",
            tags$b("In progress."),
            " The site is planned, awarded, or under construction. The live check watches its ",
            "PlugShare page; when it stops saying \"Coming Soon\", it is ", tags$b("flagged for review"),
            " so a person can verify it. Coming-Soon sites also carry a confidence level.")),
        div(class = "about-tier tier-b",
          div(class = "about-tier-tag", "Open"),
          div(class = "about-tier-body",
            tags$b("Live."),
            " Once verified on PlugShare, click ", tags$b("Confirm operational"),
            " and the station is marked open. Coming Soon, Awarded, and Open sites all now carry a ",
            "PlugShare link, so any of them can be checked against its live listing."))),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("pencil-square"), tags$span("Adding a station")),
        tags$ol(class = "about-steps",
          tags$li("Open ", tags$b("Add Station"), " and complete the required fields: name, address, ",
                  "state, latitude, longitude."),
          tags$li("Choose a ", tags$b("Station type"), ". For Coming Soon, also set a ",
                  tags$b("Confidence level"), " - hover the info icon for what each level means."),
          tags$li("Optionally add the ", tags$b("PlugShare location ID"), " so the live check can watch it."),
          tags$li("Click ", tags$b("Save"), ". It is stored durably and appears immediately on the ",
                  "map and in the tracker."))),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("bar-chart-steps"), tags$span("Confidence levels (Coming Soon)")),
        div(class = "about-conf",
          div(class = "about-conf-row",
              tags$span(class = "conf-dot", style = "background:#990000"),
              tags$b("High Confidence (Constructed)"), tags$span("Site is constructed but not yet powered on.")),
          div(class = "about-conf-row",
              tags$span(class = "conf-dot", style = "background:#ffc107"),
              tags$b("Medium Confidence (Plans Exist)"), tags$span("Plans exist.")),
          div(class = "about-conf-row",
              tags$span(class = "conf-dot", style = "background:#6c757d"),
              tags$b("Low Confidence (Announced)"), tags$span("Location announced.")))),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("palette-fill"), tags$span("Map colors")),
        tags$p(class = "about-lead",
          tags$b("Status:"), " Operational (green), Awarded (navy), ",
          "Needs Review (amber), Existing DCFC (charcoal, off by default). ", tags$b("Coming-Soon stations"),
          " are colored by confidence - High/Constructed (crimson), Medium/Plans Exist (yellow), ",
          "Low/Announced (gray) - with their own legend on the map. The state outline is black."),
        tags$p(class = "about-lead",
          tags$b("Alternative Fuel Corridors:"), " the FHWA-designated NEVI corridors for Tennessee ",
          "(I-24, I-26, I-40, I-65, I-75, I-81, US-64) draw as a dark-blue overlay you can toggle in the ",
          "map's layers control.")),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("hdd-network-fill"), tags$span("Durable & live")),
        tags$ul(class = "about-list",
          tags$li(tags$b("Durable edits:"), " edits and added stations are saved to a SQLite store ",
                  "so they persist between visits, and can be published to the TDOT ArcGIS web map ",
                  "(see ", tags$b("ArcGIS web map sync"), " below)."),
          tags$li(tags$b("Live auto-update:"), " ", tags$b("Check all stations now"),
                  " re-reads each Coming Soon station's PlugShare page and flags any that stop ",
                  "saying \"Coming Soon\"."),
          tags$li(tags$b("Permanent record:"), " confirmed operational stations should be promoted to ",
                  tags$code("master_Data_TN.csv"), "."))),
      div(class = "about-sec",
        div(class = "about-h", bs_icon("globe-americas"), tags$span("ArcGIS web map sync")),
        tags$p(class = "about-lead",
          "This tracker is the source of truth for creditable stations, and it can publish them ",
          "straight to the public TDOT ", tags$b("ArcGIS Online web map"), " so the two never drift apart."),
        tags$ul(class = "about-list",
          tags$li(tags$b("What syncs:"), " the ", tags$b("Creditable Stations (Open)"), " and ",
                  tags$b("Creditable Stations (Coming Soon)"), " layers — the same layers shown on the ",
                  "web map. Confirming a station operational here moves it from Coming Soon to Open ",
                  "on the map too."),
          tags$li(tags$b("When it syncs:"), " automatically when you click ",
                  tags$b("Confirm operational"), ", or on demand via the sidebar ",
                  tags$b("Sync to ArcGIS map"), " button."),
          tags$li(tags$b("Safe by design:"), " the public sees a ", tags$b("read-only"), " copy of ",
                  "the layer; only this tool, with its own credentials, can write. Nothing syncs ",
                  "until an administrator switches it on."))),
      div(class = "about-note", bs_icon("info-circle-fill"),
        tags$span("PlugShare blocks iframe embedding, so the PlugShare tab recreates the listing with ",
                  "an ESRI map plus the data we hold, and deep-links to the live page.")))))
)

# ---- Login gate UI ----------------------------------------------------------
# INDOT-themed login rendered as a FIXED FULL-SCREEN OVERLAY baked into the STATIC UI
# (so it paints on the very first frame — no flash of the app behind it). The server
# only toggles the body class `app-authed`, which CSS uses to hide the overlay. The
# real app underneath is the untouched main_ui, so its bslib layout is never disturbed.
login_overlay <- div(class = "login-wrap", id = "login-overlay",
  div(class = "login-card",
    div(class = "login-logos",
      tags$img(src = "TDOT_logo.png", class = "login-logo", alt = "Tennessee Department of Transportation"),
      tags$span(class = "logo-chip", tags$img(src = "hntb_logo.png", class = "login-logo", alt = "HNTB"))),
    div(class = "login-title", "TEVI Tracker"),
    div(class = "login-sub", "Tennessee NEVI Program"),
    div(class = "login-body",
      tags$label("Username", class = "login-label", `for` = "login_user"),
      textInput("login_user", NULL, placeholder = "Username", width = "100%"),
      tags$label("Password", class = "login-label", `for` = "login_pass"),
      passwordInput("login_pass", NULL, placeholder = "Password", width = "100%"),
      uiOutput("login_err_ui"),
      actionButton("login_btn", "Sign in", class = "btn login-btn w-100"),
      div(class = "login-foot", "TDOT · HNTB"))))

# Top-level UI: the REAL app (main_ui, untouched) with the login overlay baked in as
# static markup. CSS hides main content + shows the overlay UNTIL the body gets the
# `app-authed` class (added by the server on successful login, removed on logout).
app_ui <- tagList(
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  main_ui,
  login_overlay,
  tags$script(htmltools::HTML(paste0(
    "document.addEventListener('keydown', function(e){",
    "  if(e.key==='Enter' && !document.body.classList.contains('app-authed')){",
    "    var b=document.getElementById('login_btn'); if(b){ b.click(); } }",
    "});",
    "if (window.Shiny) {",
    "  Shiny.addCustomMessageHandler('authToggle', function(m){",
    "    if (m && m.authed) { document.body.classList.add('app-authed'); }",
    "    else { document.body.classList.remove('app-authed'); ",
    "           var u=document.getElementById('login_user'); if(u){ u.focus(); } }",
    "  });",
    "}")))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  # ---- Login gate ----------------------------------------------------------
  # The overlay is baked into the static UI (no flash). We hide it by toggling the
  # body class `app-authed` (CSS does the showing/hiding). Login adds it; logout removes it.
  authed <- reactiveVal(FALSE)
  login_err <- reactiveVal(NULL)
  auto_checked <- reactiveVal(FALSE)   # ensures the auto live-check runs once per session
  output$login_err_ui <- renderUI({
    if (is.null(login_err())) NULL else div(class = "login-error", login_err())
  })
  observeEvent(input$login_btn, {
    ok <- identical(trimws(input$login_user %||% ""), AUTH_USER) &&
          identical(input$login_pass %||% "", AUTH_PASS)
    if (ok) {
      authed(TRUE); login_err(NULL)
      session$sendCustomMessage("authToggle", list(authed = TRUE))
      # Run the live PlugShare check automatically on first sign-in this session, so
      # "Needs Review" is populated immediately — no need to click "Check all stations".
      if (!isTRUE(auto_checked())) {
        auto_checked(TRUE)
        ids <- merged() %>% filter(location_id != "")
        if (nrow(ids) > 0) {
          res <- run_checks(ids)
          if (length(res$flagged) > 0)
            showNotification(HTML(paste0("<b>", length(res$flagged),
              " station(s) flagged for review.</b><br><span style='font-size:11px'>",
              "Open the <b>Review</b> tab to verify and confirm.</span>")),
              type = "warning", duration = 10)
          else
            showNotification("Live check complete — no stations currently need review.",
              type = "message", duration = 6)
        }
      }
    } else {
      login_err("Incorrect username or password.")
    }
  })
  observeEvent(input$logout_btn, {
    authed(FALSE); login_err(NULL)
    updateTextInput(session, "login_user", value = "")
    updateTextInput(session, "login_pass", value = "")
    session$sendCustomMessage("authToggle", list(authed = FALSE))
  })

  rv <- reactiveValues(track = read_tracking())
  # The station universe (identity rows). Starts as BASE — which already includes any
  # custom stations loaded at startup — and grows when one is added at runtime, so the
  # map/tracker/review update without a page reload.
  stations_rv <- reactiveVal(BASE)

  merged <- reactive({
    m <- stations_rv()[, c("station_id","station_name","address","state","lat","lon",
                  "location_id","plugshare_url","master_operational","open_date","is_custom",
                  "confidence_level","data_source")]
    m <- dplyr::left_join(m, rv$track, by = "station_id")
    if (!"review_flag" %in% names(m)) m$review_flag <- ""
    m$review_flag[is.na(m$review_flag)] <- ""
    m$cls <- status_class(m$operational, m$ps_status)
    m$disp <- status_label(m$operational, m$ps_status)
    # A flagged candidate is still "Coming Soon" underneath, but we surface it as amber
    # "Needs review" everywhere it's shown, so it stands out without changing the status.
    rv_flag <- m$review_flag == "candidate" & m$cls == "cs"
    m$cls[rv_flag] <- "rv"
    m$disp[rv_flag] <- "Needs review"
    # Stations flagged operational in master_Data.csv (Schmidt, Pilot Greenfield, BIC) are shown
    # GREEN/"Operational" everywhere — map marker, KPI count, and Tracker row — for consistency.
    # DCFCs and Awarded keep their own class even though DCFCs are technically "open".
    op_flag <- isTRUE_vec(m$master_operational) & !(m$cls %in% c("dcfc","awarded"))
    m$cls[op_flag] <- "op"
    m$disp[op_flag] <- "Operational"
    # User-added stations still in plain Coming-Soon state get their own "New Coming-Soon"
    # class (violet) — distinct from master Coming Soon, but they still flip to amber when
    # flagged and green when confirmed (handled above, so this only catches the leftover cs).
    nc_flag <- isTRUE_vec(m$is_custom) & m$cls == "cs"
    m$cls[nc_flag] <- "nc"
    m$disp[nc_flag] <- "New Coming Soon"
    m$color <- vapply(m$cls, status_color, character(1))
    # Added ("custom") stations get a distinct VIOLET RING so they read as user-added,
    # and Coming-Soon customs are FILLED by confidence (match the Scenario palette:
    # High=green, Medium=amber, Low=gray). Master stations keep the neutral dark ring.
    m$border  <- ifelse(isTRUE_vec(m$is_custom), "#FFFFFF", "#333333")
    m$mweight <- ifelse(isTRUE_vec(m$is_custom), 2.5, 1)
    m$mradius <- ifelse(m$cls == "dcfc", 4, 8)   # DCFC context dots are small
    m$border[m$cls == "dcfc"] <- "#3a3a3a"; m$mweight[m$cls == "dcfc"] <- 0.5
    # Coming-Soon stations (master `cs` OR custom `nc`) are FILLED by confidence
    # (High=crimson, Medium=yellow, Low=gray). Custom ones also get the white ring.
    conf_flag <- (m$cls == "cs" | m$cls == "nc") &
                 !is.na(m$confidence_level) & m$confidence_level != ""
    if (any(conf_flag)) m$color[conf_flag] <- conf_color(m$confidence_level[conf_flag])
    m
  })

  filtered <- reactive({
    m <- merged()
    keep <- function(d) {
      if (d == "Operational") "Operational" %in% input$flt_status
      else if (d == "Awarded") "Awarded" %in% input$flt_status
      else if (grepl("Existing", d)) "Existing DCFC" %in% input$flt_status
      else if (grepl("Repair", d)) "Under Repair" %in% input$flt_status
      else if (grepl("Not found", d)) "Not found" %in% input$flt_status
      else "Coming Soon" %in% input$flt_status   # Coming Soon / New Coming Soon / Needs review
    }
    m[vapply(m$disp, keep, logical(1)) & m$state %in% input$flt_state, ]
  })

  # KPIs (over all tracked, not filtered). cls is authoritative: master_operational stations
  # (Schmidt, Pilot Greenfield, BIC) are already forced to "op" in merged(), so counts, map
  # color, and Tracker rows all agree.
  # "Tracked" counts only the tracked layers (Coming Soon, Awarded, Open) - NOT DCFC context.
  output$kpi_total <- renderText({ m <- merged(); sum(m$cls != "dcfc", na.rm = TRUE) })
  output$kpi_op <- renderText({ m <- merged(); sum(m$cls == "op", na.rm = TRUE) })
  output$kpi_cs <- renderText({ m <- merged(); sum(m$cls == "cs", na.rm = TRUE) })
  output$kpi_nc <- renderText({ m <- merged(); sum(m$cls == "nc", na.rm = TRUE) })
  output$kpi_ur <- renderText({ m <- merged(); sum(m$cls == "ur", na.rm = TRUE) })
  output$kpi_nf <- renderText({ m <- merged(); sum(m$cls == "nf", na.rm = TRUE) })
  output$kpi_awarded <- renderText({ m <- merged(); sum(m$cls == "awarded", na.rm = TRUE) })
  output$kpi_dcfc <- renderText({ m <- merged(); sum(m$cls == "dcfc", na.rm = TRUE) })
  output$last_check_txt <- renderText({
    lc <- rv$track$last_checked; lc <- lc[lc != "" & !is.na(lc)]
    if (length(lc) == 0) "No live check yet." else paste("Last live check:", max(lc))
  })

  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "ESRI Streets") %>%
      addProviderTiles(providers$Esri.WorldTopoMap,  group = "ESRI Topo") %>%
      addProviderTiles(providers$Esri.WorldImagery,  group = "ESRI Imagery") %>%
      addPolylines(lng = BS_BORDER$x, lat = BS_BORDER$y, color = "#9aa5b5", weight = 1,
                   opacity = .6, group = "State borders") %>%
      add_indiana(fill = TRUE) %>%
      {if (!is.null(AFC_TN)) addPolylines(., data = AFC_TN, color = "#12408A",
        weight = 3.5, opacity = .75, group = "EV AFC",
        label = ~PRIMARY_NA) else .} %>%
      addLayersControl(baseGroups = c("ESRI Streets","ESRI Topo","ESRI Imagery"),
                       overlayGroups = if (!is.null(AFC_TN)) "EV AFC" else NULL,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      fitBounds(IN_BBOX$xmin, IN_BBOX$ymin, IN_BBOX$xmax, IN_BBOX$ymax)
  })
  observe({
    m <- filtered(); m <- m[!is.na(m$lat) & !is.na(m$lon), ]
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearControls()
    if (nrow(m) > 0) {
      proxy %>% addCircleMarkers(lng = m$lon, lat = m$lat, radius = m$mradius, color = m$border,
        weight = m$mweight, fillColor = m$color, fillOpacity = .9,
        popup = vapply(seq_len(nrow(m)), function(i) make_popup(m[i, ]), character(1)),
        label = lapply(m$station_name, HTML)) %>%
        addLegend("topright",
          colors = c(INDOT$green, INDOT$navy, INDOT$amber, "#3a3a3a"),
          labels = c("Creditable Stations (Open)","TEVI Round 1 Award Stations","Needs Review","Existing DCFC"),
          title = "Status", opacity = .9, className = "info legend status-legend")
      # Coming-Soon stations are colored by CONFIDENCE (not one flat status color), so their
      # key lives in its own legend — shown whenever any coming-soon (master `cs` or added `nc`)
      # is on the map. This is what keeps the map colors matching the legend.
      if (any(m$cls %in% c("cs","nc"), na.rm = TRUE)) {
        proxy %>% addLegend("topright",   # stacks directly under the Status legend
          colors = c(CONF_COLORS[["High"]], CONF_COLORS[["Medium"]], CONF_COLORS[["Low"]]),
          labels = c(conf_label("High"), conf_label("Medium"), conf_label("Low")),
          title = "Creditable (Coming Soon) — confidence", opacity = .9,
          className = "info legend conf-legend")
      }
    }
  })

  # ---- Review tab map: amber pins for the stations flagged for review ----
  # Draw the flagged-station pins INSIDE renderLeaflet (not a separate observe), so they
  # appear when the Review tab becomes visible. A click opens a rich popup with verify info.
  output$review_map <- renderLeaflet({
    rdf <- review_df()
    rdf <- rdf[!is.na(rdf$lat) & !is.na(rdf$lon), , drop = FALSE]
    map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      add_indiana(fill = TRUE)
    if (nrow(rdf) == 0)
      return(map %>% fitBounds(IN_BBOX$xmin, IN_BBOX$ymin, IN_BBOX$xmax, IN_BBOX$ymax))
    popups <- vapply(seq_len(nrow(rdf)), function(i) make_review_popup(rdf[i, ]), character(1))
    map <- map %>% addCircleMarkers(lng = rdf$lon, lat = rdf$lat, radius = 10,
      color = "#7a5600", weight = 1.5, fillColor = INDOT$amber, fillOpacity = .92,
      label = lapply(rdf$station_name, HTML), popup = popups,
      popupOptions = popupOptions(className = "rv-popup", maxWidth = 320))
    if (nrow(rdf) == 1)
      map %>% setView(rdf$lon[1], rdf$lat[1], zoom = 9)
    else
      map %>% fitBounds(min(rdf$lon) - .3, min(rdf$lat) - .3, max(rdf$lon) + .3, max(rdf$lat) + .3)
  })

  # ---- Tracker ----
  dt_df <- reactive({
    m <- merged()
    if (!is.null(input$tbl_layer) && input$tbl_layer != "all")
      m <- m[!is.na(m$data_source) & m$data_source == input$tbl_layer, , drop = FALSE]
    # The displayed "Operational" column reflects the TRUE operational state: a station counts as
    # operational if flagged in master_Data.csv (Schmidt, Pilot Greenfield, BIC) or confirmed in the
    # tracker. This is what drives the green row highlight below (so BIC's row is green too, even
    # though its map marker stays red = Under Repair).
    m$operational <- ifelse(isTRUE_vec(m$master_operational) |
                              (!is.na(m$operational) & m$operational == "Yes"),
                            "Yes", ifelse(is.na(m$operational), "", m$operational))
    df <- m[, c("station_id","station_name","state","disp","ps_status","operational",
                "network","chargers","notes","plugshare_url")]
    df$plugshare <- ifelse(df$plugshare_url != "",
      sprintf("<a href='%s' target='_blank' rel='noopener'>PlugShare &#8599;</a>", df$plugshare_url),
      "<span style='color:#999'>not listed</span>")
    df$plugshare_url <- NULL
    names(df) <- c("station_id","Station","State","Status","PlugShare status","Operational",
                   "Network","Ports","Notes","PlugShare")
    df
  })
  output$tbl <- renderDT({
    df <- dt_df()
    ed <- which(names(df) %in% c("PlugShare status","Operational","Network","Ports","Notes")) - 1
    op_col <- which(names(df) == "Operational") - 1  # 0-based column index for the JS callback
    # Tag OPERATIONAL rows with a CSS class via rowCallback. A class (styled with !important in
    # styles.css) wins permanently over DataTables' hover/stripe styles — so the whole row stays
    # light green at all times, not just on hover. Text stays normal/black like other rows.
    dt <- datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
      editable = list(target = "cell", disable = list(columns = setdiff(seq_len(ncol(df)) - 1, ed))),
      options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
        columnDefs = list(list(visible = FALSE, targets = 0)),
        rowCallback = DT::JS(sprintf(
          "function(row, data) { if (data[%d] === 'Yes') { $(row).addClass('op-row-green'); } }",
          op_col))),
      class = "row-border hover")
    dt
  }, server = FALSE)
  field_map <- c("PlugShare status"="ps_status","Operational"="operational",
                 "Network"="network","Ports"="chargers","Notes"="notes")
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit; df <- dt_df(); cn <- names(df)[info$col + 1]
    if (cn %in% names(field_map)) {
      update_cell(df$station_id[info$row], field_map[[cn]], info$value)
      rv$track <- read_tracking()
    }
  })
  observeEvent(input$refresh, { rv$track <- read_tracking() })

  # Manual full re-publish to the ArcGIS web map (button only present when enabled).
  observeEvent(input$sync_arcgis, {
    showNotification("Syncing to the ArcGIS web map…", id = "arcsync", duration = NULL)
    res <- tryCatch(sync_to_arcgis(), error = function(e) e)
    removeNotification("arcsync")
    if (inherits(res, "error"))
      showNotification(paste("ArcGIS sync failed:", conditionMessage(res)),
                       type = "error", duration = 10)
    else if (isFALSE(res))
      showNotification("ArcGIS sync is not enabled (set ARC_SYNC_ENABLED + credentials).",
                       type = "warning", duration = 8)
    else
      showNotification("Creditable stations published to the ArcGIS web map.",
                       type = "message", duration = 6)
  }, ignoreInit = TRUE)
  output$dl_csv <- downloadHandler(
    filename = function() "coming_soon_tracker_export.csv",
    content = function(file) {
      m <- merged()
      out <- m[, c("station_name","address","state","disp","ps_status","operational",
                   "network","plugs","chargers","open_date","notes","location_id","plugshare_url","last_checked")]
      names(out) <- c("Station","Address","State","Status","PlugShare_Status","Operational",
                      "Network","Plugs","Ports","Open_Date","Notes","PlugShare_ID","PlugShare_URL","Last_Checked")
      write.csv(out, file, row.names = FALSE)
    })

  # ---- Live check (FLAG for review — never auto-flip) ----
  # Returns counts so the caller can craft a message. A "candidate" is a station whose
  # PlugShare title no longer says "Coming Soon" AND is a specific listing (not the
  # generic homepage title). It is FLAGGED, not promoted: a person confirms it.
  run_checks <- function(ids_df) {
    flagged <- character(0); inconclusive <- 0L; n <- nrow(ids_df)
    withProgress(message = "Checking PlugShare…", value = 0, {
      for (i in seq_len(n)) {
        r <- ids_df[i, ]
        incProgress(1 / n, detail = r$station_name)
        res <- scrape_plugshare_status(r$location_id)
        if (!isTRUE(res$ok)) { inconclusive <- inconclusive + 1L; next }
        cur_cs <- status_class(r$operational, r$ps_status) == "cs"
        if (res$outcome == "candidate" && cur_cs) {
          flagged <- c(flagged, r$station_name)
          flag_review(r$station_id)         # raise the amber flag; status stays Coming Soon
        } else if (res$outcome == "coming_soon") {
          touch_checked(r$station_id)        # still coming soon — just record we looked
        } else {
          inconclusive <- inconclusive + 1L  # generic/blank title — DON'T act (avoids false positives)
          touch_checked(r$station_id)
        }
      }
    })
    rv$track <- read_tracking()
    list(flagged = flagged, inconclusive = inconclusive)
  }
  observeEvent(input$check_all, {
    ids <- merged() %>% filter(location_id != "")
    res <- run_checks(ids)
    if (length(res$flagged) == 0)
      showNotification(sprintf("Checked PlugShare — no new candidates.%s",
        if (res$inconclusive > 0) sprintf(" (%d page(s) returned no clear status.)", res$inconclusive) else ""),
        type = "message", duration = 7)
    else
      showNotification(HTML(paste0(
        "<b>", length(res$flagged), " station(s) flagged for review:</b><br>",
        paste(htmlEscape(res$flagged), collapse = "<br>"),
        "<br><span style='font-size:11px'>PlugShare no longer lists these as \"Coming Soon\". ",
        "<b>Verify on PlugShare</b>, then click <b>Confirm operational</b> in the Review tab. ",
        "Nothing was changed automatically.</span>")),
        type = "warning", duration = NULL)
  })
  # "Run live check" button on the Review tab triggers the same scan as the sidebar.
  observeEvent(input$check_all2, {
    ids <- merged() %>% filter(location_id != "")
    res <- run_checks(ids)
    if (length(res$flagged) == 0)
      showNotification("Checked PlugShare — no new candidates.", type = "message", duration = 6)
    else
      showNotification(HTML(paste0("<b>", length(res$flagged),
        " station(s) flagged below for review.</b>")), type = "warning", duration = 8)
  })
  observeEvent(input$check_one, {
    s <- sel_station(); req(!is.null(s), nrow(s) == 1, s$location_id != "")
    res <- run_checks(s)
    showNotification(
      if (length(res$flagged))
        HTML(paste0("<b>", htmlEscape(s$station_name), "</b> may have gone live — ",
                    "flagged for review. Verify on PlugShare, then confirm."))
      else if (res$inconclusive)
        paste0(s$station_name, ": PlugShare returned no clear status (try again later).")
      else paste0(s$station_name, ": still Coming Soon."),
      type = if (length(res$flagged)) "warning" else "message", duration = 8)
  })

  # ---- Review queue: confirm / dismiss flagged candidates ----
  review_df <- reactive({
    m <- merged()
    m[!is.na(m$review_flag) & m$review_flag == "candidate", , drop = FALSE]
  })
  # Stations confirmed via THIS tracker (ps_status set by confirm_operational). These
  # get an "undo" control so a test confirmation can be rolled back cleanly. Stations
  # operational from master_Data.csv have a different ps_status and never appear here.
  confirmed_df <- reactive({
    m <- merged()
    m[!is.na(m$ps_status) & m$ps_status == "Operational (confirmed)", , drop = FALSE]
  })
  output$kpi_review <- renderText({ nrow(review_df()) })
  output$review_ui <- renderUI({
    rdf <- review_df()
    cdf <- confirmed_df()
    # --- Part A: stations awaiting review (the candidate queue) ---
    review_part <- if (nrow(rdf) == 0)
      div(class = "review-empty",
        svg_to_tag("check", INDOT$green, 26),
        tags$div(tags$b("No stations awaiting review."),
                 tags$div(class = "muted", "Run “Check all stations now” to scan PlugShare. ",
                          "Any station that stops showing “Coming Soon” appears here for you to verify.")))
    else
    tagList(lapply(seq_len(nrow(rdf)), function(i) {
      r <- rdf[i, ]
      div(class = "review-card",
        div(class = "rc-head",
          span(class = "rc-badge", "NEEDS REVIEW"),
          span(class = "rc-badge rc-layer", toupper(type_label(r$data_source))),
          div(class = "rc-headtext",
              tags$div(class = "rc-name", r$station_name),
              tags$div(class = "rc-addr", r$address))),
        div(class = "rc-body",
          tags$div(class = "rc-meta",
            tags$span(class = "rc-chip", sprintf("Network: %s", ifelse(is.na(r$network) || r$network=="", "—", r$network))),
            tags$span(class = "rc-chip", sprintf("State: %s", ifelse(is.na(r$state) || r$state=="", "—", r$state))),
            tags$span(class = "rc-chip", sprintf("PlugShare ID: %s", ifelse(r$location_id=="", "—", r$location_id))),
            if (!is.na(r$open_date) && r$open_date != "")
              tags$span(class = "rc-chip", sprintf("Open date: %s", r$open_date)),
            if (!is.na(r$review_at) && r$review_at != "")
              tags$span(class = "rc-chip", sprintf("Flagged: %s", substr(r$review_at,1,16)))),
          tags$div(class = "rc-steps",
            tags$div(class = "rc-step",
              tags$span(class = "rc-stepnum", "1"),
              tags$div(tags$b("Verify on PlugShare."),
                " Open the listing and confirm it is genuinely live — chargers present and recent check-ins.")),
            tags$div(class = "rc-step",
              tags$span(class = "rc-stepnum", "2"),
              tags$div(tags$b("Confirm."),
                " This marks the station Operational in the tracker.")))),
        div(class = "rc-actions",
          if (r$plugshare_url != "")
            tags$a(class = "btn btn-sm btn-outline-primary", href = r$plugshare_url,
                   target = "_blank", rel = "noopener", HTML("&#9312; Verify on PlugShare &#8599;")),
          actionButton(paste0("confirm_", r$station_id), HTML("&#9313; Confirm operational"),
                       class = "btn btn-sm btn-success"),
          actionButton(paste0("dismiss_", r$station_id), "Still coming soon",
                       class = "btn btn-sm btn-outline-secondary")))
    }))
    # --- Part B: confirmed this session — one-click undo (test/demo reset) ---
    undo_part <- if (nrow(cdf) > 0)
      tagList(
        tags$hr(style = "margin:18px 0"),
        tags$div(class = "rc-headtext", style = "margin-bottom:10px",
          tags$div(class = "rc-name", "Confirmed operational"),
          tags$div(class = "muted",
            "Reset a station to send it back to “needs review” and mark it Coming Soon again ",
            "in the tracker. Use this to undo a test confirmation before a demo.")),
        lapply(seq_len(nrow(cdf)), function(i) {
          r <- cdf[i, ]
          div(class = "review-card",
            div(class = "rc-head",
              span(class = "rc-badge", style = "background:#e8f5e9;color:#1b5e20", "OPERATIONAL"),
              div(class = "rc-headtext",
                  tags$div(class = "rc-name", r$station_name),
                  tags$div(class = "rc-addr", r$address))),
            div(class = "rc-actions",
              actionButton(paste0("reset_", r$station_id),
                           HTML("&#8634; Reset to Coming Soon"),
                           class = "btn btn-sm btn-outline-warning")))
        }))
    else NULL
    tagList(review_part, undo_part)
  })
  # Confirm / dismiss / reset handlers, wired ONCE per station id (guarded by a registry
  # so they never stack — and so a station ADDED at runtime can be wired on the fly).
  wired_env <- new.env()
  wire_station <- function(id) {
    if (!is.null(wired_env[[id]])) return(invisible())
    wired_env[[id]] <- TRUE
    observeEvent(input[[paste0("confirm_", id)]], {
      row <- merged()[merged()$station_id == id, ][1, ]   # the confirmed station's row
      nm  <- row$station_name
      confirm_operational(id); rv$track <- read_tracking() # existing: save locally
      post_operational(id, row$address,                    # push to the shared status API
                       if (!is.null(row$open_date)) row$open_date else "")
      tryCatch(sync_to_arcgis(),                           # STAGED: no-op unless ARC_SYNC_ENABLED=true
               error = function(e) showNotification(
                 paste("ArcGIS sync failed:", conditionMessage(e)), type = "error"))
      showNotification(HTML(paste0("<b>", htmlEscape(nm), "</b> confirmed Operational.<br>",
        "<span style='font-size:11px'>Saved to the tracker &amp; pushed to the status API — ",
        "promote to master_Data.csv after review.</span>")),
        type = "message", duration = 8)
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("dismiss_", id)]], {
      # "Still coming soon" = NOT operational, so also clear it from the API (safe
      # no-op if it was never there) — keeps the Scenario tool in sync either way.
      clear_review(id); delete_operational(id); rv$track <- read_tracking()
    }, ignoreInit = TRUE)
    # Reset a confirmed station: undo locally AND remove it from the shared API.
    observeEvent(input[[paste0("reset_", id)]], {
      row <- merged()[merged()$station_id == id, ][1, ]
      nm  <- row$station_name
      reset_review(id); rv$track <- read_tracking()  # local: back to "needs review"
      delete_operational(id)                         # API: drop it (Scenario clears on refresh)
      showNotification(HTML(paste0("<b>", htmlEscape(nm), "</b> reset to Coming Soon.<br>",
        "<span style='font-size:11px'>Returned to the review queue &amp; removed from the status API — ",
        "the Scenario tool drops it on its next refresh.</span>")),
        type = "warning", duration = 8)
    }, ignoreInit = TRUE)
  }
  for (sid in BASE$station_id) wire_station(sid)

  # ---- Add Station form -----------------------------------------------------
  ns_msg <- reactiveVal(NULL)
  output$ns_msg <- renderUI({
    m <- ns_msg(); if (is.null(m)) return(NULL)
    div(class = paste0("ns-alert ns-", m$type), HTML(m$text))
  })
  ns_reset_form <- function() {
    updateTextInput(session, "ns_name", value = "")
    updateTextInput(session, "ns_address", value = "")
    updateNumericInput(session, "ns_lat", value = NA)
    updateNumericInput(session, "ns_lon", value = NA)
    updateTextInput(session, "ns_psid", value = "")
    updateTextInput(session, "ns_network", value = "")
    updateTextInput(session, "ns_plugs", value = "")
    updateTextInput(session, "ns_chargers", value = "")
    updateTextInput(session, "ns_open", value = "")
    updateTextAreaInput(session, "ns_notes", value = "")
    updateSelectInput(session, "ns_type", selected = "Coming_Soon")
    updateSelectInput(session, "ns_conf", selected = "Medium")
  }
  observeEvent(input$ns_clear, { ns_reset_form(); ns_msg(NULL) })
  observeEvent(input$ns_save, {
    g    <- function(x) trimws(as.character(x %||% ""))
    name <- g(input$ns_name); addr <- g(input$ns_address); st <- g(input$ns_state)
    lat  <- suppressWarnings(as.numeric(input$ns_lat))
    lon  <- suppressWarnings(as.numeric(input$ns_lon))
    miss <- c(if (!nzchar(name)) "Station name", if (!nzchar(addr)) "Street address",
              if (!nzchar(st)) "State", if (is.na(lat)) "Latitude", if (is.na(lon)) "Longitude")
    if (length(miss) > 0) {
      ns_msg(list(type = "err", text = paste0("Please complete the required field(s): <b>",
                  paste(miss, collapse = ", "), "</b>."))); return()
    }
    if (lat < -90 || lat > 90 || lon < -180 || lon > 180) {
      ns_msg(list(type = "err",
        text = "Latitude must be between -90 and 90, and longitude between -180 and 180.")); return()
    }
    id <- slugify(addr)
    if (id %in% stations_rv()$station_id || custom_exists(id)) {
      ns_msg(list(type = "err",
        text = "A station with this address already exists in the tracker.")); return()
    }
    dsrc <- g(input$ns_type); if (!nzchar(dsrc)) dsrc <- "Coming_Soon"
    conf <- if (identical(dsrc, "Coming_Soon")) g(input$ns_conf) else ""
    rec <- data.frame(station_id = id, station_name = name, address = addr, state = st,
      lat = as.character(lat), lon = as.character(lon), location_id = g(input$ns_psid),
      network = g(input$ns_network), plugs = g(input$ns_plugs), chargers = g(input$ns_chargers),
      open_date = g(input$ns_open), notes = g(input$ns_notes),
      added_at = as.character(Sys.time()), data_source = dsrc, confidence_level = conf,
      stringsAsFactors = FALSE)
    ok <- tryCatch({ add_custom_station(rec); seed_tracking(rec); TRUE },
                   error = function(e) { ns_msg(list(type = "err",
                     text = paste("Save failed:", htmlEscape(conditionMessage(e))))); FALSE })
    if (!ok) return()
    api_save_station(rec)   # durable copy on the Render disk (survives restarts)
    # Coming Soon customs join the live map/review universe; other types are passthrough.
    brow <- custom_to_base(rec)
    if (!is.null(brow)) {
      stations_rv(rbind(stations_rv(), brow[, names(stations_rv())]))
      wire_station(id)
    }
    rv$track <- read_tracking()
    ns_reset_form()
    msg_tail <- if (identical(dsrc, "Coming_Soon"))
      "added to the map. It will be flagged for review automatically when PlugShare shows it operational."
    else
      paste0("sent straight to the Scenario tool's <b>", type_label(dsrc),
             "</b> layer — no approval needed.")
    ns_msg(list(type = "ok", text = paste0("<b>", htmlEscape(name),
      "</b> saved to the tracker database — ", msg_tail)))
    showNotification(HTML(paste0("<b>", htmlEscape(name), "</b> added to the tracker.")),
                     type = "message", duration = 5)
  })
  output$ns_recent <- renderUI({
    rv$track  # re-render after each add / edit / delete
    cs <- read_custom_stations()
    if (nrow(cs) == 0) return(div(class = "ns-empty", "No stations added yet."))
    cs <- cs[order(cs$added_at, decreasing = TRUE), , drop = FALSE]
    rows <- lapply(seq_len(nrow(cs)), function(i) {
      r <- cs[i, ]
      tags$tr(
        tags$td(
          tags$div(class = "ns-tname", r$station_name),   # tags$ auto-escapes; no htmlEscape
          tags$div(class = "ns-taddr", r$address),
          tags$div(class = "ns-tmeta",
            tags$span(class = "ns-typechip", type_label(r$data_source)),
            if (identical(as.character(r$data_source), "Coming_Soon") &&
                nzchar(as.character(r$confidence_level %||% "")))
              tags$span(class = "ns-confchip",
                        style = paste0("background:", conf_color(r$confidence_level), ";color:",
                                       ifelse(r$confidence_level == "Medium", "#3a3000", "#fff")),
                        r$confidence_level))),
        tags$td(class = "ns-tc", r$state),
        tags$td(class = "ns-tc", substr(as.character(r$added_at), 1, 10)),
        tags$td(class = "ns-tactions",
          actionButton(paste0("cedit_", r$station_id), bs_icon("pencil-square"),
                       class = "btn btn-xs btn-edit", title = "Edit"),
          actionButton(paste0("cdel_", r$station_id), bs_icon("trash3"),
                       class = "btn btn-xs btn-del", title = "Delete")))
    })
    tags$table(class = "ns-table",
      tags$thead(tags$tr(tags$th("Station"), tags$th("State"), tags$th("Added"), tags$th(""))),
      tags$tbody(rows))
  })

  # Wire Edit/Delete handlers for each custom station (guarded, like wire_station),
  # and re-run when the custom set changes so newly-added stations get wired too.
  wired_custom <- new.env()
  wire_custom <- function(id) {
    if (!is.null(wired_custom[[id]])) return(invisible())
    wired_custom[[id]] <- TRUE
    # --- Delete: confirm, then remove from DB, map, and review queue ---
    observeEvent(input[[paste0("cdel_", id)]], {
      cs <- read_custom_stations(); r <- cs[cs$station_id == id, ]
      nm <- if (nrow(r)) r$station_name[1] else id
      showModal(modalDialog(title = "Delete station?",
        tags$p(HTML(paste0("Remove <b>", htmlEscape(nm), "</b> from the tracker entirely? ",
          "This deletes it from the database, the map, and the review queue — it cannot be undone."))),
        footer = tagList(modalButton("Cancel"),
          actionButton(paste0("cdelok_", id), "Delete", class = "btn btn-danger")),
        easyClose = TRUE))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("cdelok_", id)]], {
      delete_custom_station(id); delete_operational(id)   # also clear API if ever confirmed
      api_delete_station(id)                               # remove from the durable registry too
      su <- stations_rv(); stations_rv(su[su$station_id != id, , drop = FALSE])
      rv$track <- read_tracking(); removeModal()
      showNotification("Station deleted.", type = "message", duration = 4)
    }, ignoreInit = TRUE)
    # --- Edit: open a dialog pre-filled with the station's fields ---
    observeEvent(input[[paste0("cedit_", id)]], {
      cs <- read_custom_stations(); r <- cs[cs$station_id == id, ]
      if (!nrow(r)) return(); r <- r[1, ]
      showModal(modalDialog(title = tagList(bs_icon("pencil-square"), " Edit station"), size = "l",
        div(class = "addform",
          layout_columns(col_widths = c(12),
            textInput(paste0("e_name_", id), HTML('Station name <span class="req">*</span>'),
                      value = r$station_name)),
          layout_columns(col_widths = c(4, 4, 4),
            selectInput(paste0("e_state_", id), "State", choices = c("TN","KY","VA","NC","GA","AL","MS","AR","MO"),
                        selected = r$state),
            textInput(paste0("e_lat_", id), HTML('Latitude <span class="req">*</span>'), value = r$lat),
            textInput(paste0("e_lon_", id), HTML('Longitude <span class="req">*</span>'), value = r$lon)),
          layout_columns(col_widths = c(6, 6),
            selectInput(paste0("e_type_", id), "Station type",
                        choices = c("Coming Soon" = "Coming_Soon",
                                    "NEVI Awarded" = "NEVI Awarded Sites",
                                    "Open (Creditable)" = "Open_Creditable",
                                    "Existing DCFC" = "Other_DCFC"),
                        selected = if (nzchar(r$data_source %||% "")) r$data_source else "Coming_Soon"),
            conditionalPanel(paste0("input['e_type_", id, "'] == 'Coming_Soon'"),
              selectInput(paste0("e_conf_", id), "Confidence",
                          choices = CONF_CHOICES,
                          selected = if (nzchar(r$confidence_level %||% "")) r$confidence_level else "Medium"))),
          layout_columns(col_widths = c(6, 6),
            textInput(paste0("e_psid_", id), "PlugShare location ID", value = r$location_id),
            textInput(paste0("e_net_", id), "Network", value = r$network)),
          layout_columns(col_widths = c(4, 4, 4),
            textInput(paste0("e_plugs_", id), "Connectors", value = r$plugs),
            textInput(paste0("e_chg_", id), "Ports / chargers", value = r$chargers),
            textInput(paste0("e_open_", id), "Expected open date", value = r$open_date)),
          textAreaInput(paste0("e_notes_", id), "Notes", value = r$notes, height = "70px"),
          tags$div(class = "muted", style = "font-size:12px",
            "Address can't be edited (it is the station's key). Delete and re-add to change it.")),
        footer = tagList(modalButton("Cancel"),
          actionButton(paste0("ceditok_", id), "Save changes", class = "btn btn-gold")),
        easyClose = FALSE))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("ceditok_", id)]], {
      gg  <- function(s) trimws(as.character(input[[paste0(s, id)]] %||% ""))
      nm  <- gg("e_name_"); lat <- suppressWarnings(as.numeric(gg("e_lat_")))
      lon <- suppressWarnings(as.numeric(gg("e_lon_"))); st <- gg("e_state_")
      if (!nzchar(nm) || is.na(lat) || is.na(lon)) {
        showNotification("Name, latitude and longitude are required.", type = "error", duration = 5); return()
      }
      dsrc <- gg("e_type_"); if (!nzchar(dsrc)) dsrc <- "Coming_Soon"
      f <- list(station_name = nm, state = st, lat = as.character(lat), lon = as.character(lon),
                location_id = gg("e_psid_"), network = gg("e_net_"), plugs = gg("e_plugs_"),
                chargers = gg("e_chg_"), open_date = gg("e_open_"), notes = gg("e_notes_"),
                data_source = dsrc,
                confidence_level = if (identical(dsrc, "Coming_Soon")) gg("e_conf_") else "")
      update_custom_station(id, f)
      cur <- read_custom_stations(); cr <- cur[cur$station_id == id, ]
      if (nrow(cr)) api_save_station(cr[1, , drop = FALSE])   # push the edit to the durable API
      # refresh this station's identity row in the live universe
      su <- stations_rv(); i <- which(su$station_id == id)
      if (length(i) == 1) {
        su$station_name[i] <- nm; su$state[i] <- st
        su$lat[i] <- lat; su$lon[i] <- lon
        su$location_id[i] <- f$location_id
        su$plugshare_url[i] <- if (nzchar(f$location_id))
          paste0("https://www.plugshare.com/location/", f$location_id) else ""
        su$network[i] <- f$network; su$open_date[i] <- f$open_date
        su$data_source[i] <- f$data_source            # so type change re-colors live
        su$confidence_level[i] <- f$confidence_level  # so confidence re-colors live
        stations_rv(su)
      }
      rv$track <- read_tracking(); removeModal()
      showNotification(HTML(paste0("<b>", htmlEscape(nm), "</b> updated.")),
                       type = "message", duration = 4)
    }, ignoreInit = TRUE)
  }
  observe({ rv$track; for (cid in read_custom_stations()$station_id) wire_custom(cid) })

  # ---- PlugShare tab (click a marker → detail on the left + popup on the map) ----
  ps_selected <- reactiveVal(NULL)   # holds the clicked station_id (NULL = nothing selected yet)
  sel_station <- reactive({
    id <- ps_selected(); if (is.null(id)) return(NULL)
    m <- merged(); s <- m[m$station_id == id, ]
    if (nrow(s) == 0) NULL else s[1, ]
  })

  # The map is drawn ONCE: all stations + the Indiana boundary, fit to the whole state.
  # Markers carry layerId = station_id so a click tells us which station was tapped.
  output$ps_map <- renderLeaflet({
    all <- isolate(filtered()); all <- all[!is.na(all$lat) & !is.na(all$lon), ]
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "ESRI Streets") %>%
      addProviderTiles(providers$Esri.WorldTopoMap,   group = "ESRI Topo") %>%
      addProviderTiles(providers$Esri.WorldImagery,   group = "ESRI Imagery") %>%
      addLayersControl(baseGroups = c("ESRI Streets","ESRI Topo","ESRI Imagery"),
                       overlayGroups = if (!is.null(AFC_TN)) "EV AFC" else NULL,
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addPolylines(lng = BS_BORDER$x, lat = BS_BORDER$y, color = "#9aa5b5",
                   weight = 1, opacity = .7) %>%
      add_indiana(fill = TRUE) %>%
      {if (!is.null(AFC_TN)) addPolylines(., data = AFC_TN, color = "#12408A",
        weight = 3.5, opacity = .75, group = "EV AFC",
        label = ~PRIMARY_NA) else .} %>%
      # No click popup here — clicking a marker fills the sidebar detail panel instead
      # (avoids duplicating the same info on the map and in the sidebar). Hover shows the name.
      addCircleMarkers(data = all, lng = ~lon, lat = ~lat, layerId = ~station_id,
        radius = 9, color = "#fff", weight = 2, fillColor = ~color, fillOpacity = .95,
        label = lapply(paste0("<b>", htmlEscape(all$station_name), "</b><br>",
                              htmlEscape(all$disp)), HTML)) %>%
      addLegend("topright",
        colors = c(INDOT$green, INDOT$navy, INDOT$amber, "#3a3a3a"),
        labels = c("Creditable Stations (Open)","TEVI Round 1 Award Stations","Needs Review","Existing DCFC"),
        title = "Status", opacity = .9, className = "info legend status-legend") %>%
      {if (any(all$cls %in% c("cs","nc"), na.rm = TRUE))
         addLegend(., "topright",
           colors = c(CONF_COLORS[["High"]], CONF_COLORS[["Medium"]], CONF_COLORS[["Low"]]),
           labels = c(conf_label("High"), conf_label("Medium"), conf_label("Low")),
           title = "Creditable (Coming Soon) — confidence", opacity = .9,
           className = "info legend conf-legend")
       else .} %>%
      fitBounds(IN_BBOX$xmin, IN_BBOX$ymin, IN_BBOX$xmax, IN_BBOX$ymax)
  })

  # Keep marker colours in sync with status changes (flag/confirm) without redrawing the map.
  observe({
    all <- filtered(); all <- all[!is.na(all$lat) & !is.na(all$lon), ]
    leafletProxy("ps_map") %>% clearMarkers() %>%
      addCircleMarkers(data = all, lng = ~lon, lat = ~lat, layerId = ~station_id,
        radius = 9, color = "#fff", weight = 2, fillColor = ~color, fillOpacity = .95,
        label = lapply(paste0("<b>", htmlEscape(all$station_name), "</b><br>",
                              htmlEscape(all$disp)), HTML))
  })

  # A marker click selects the station: left panel updates, and we ease the map toward it.
  observeEvent(input$ps_map_marker_click, {
    clk <- input$ps_map_marker_click
    ps_selected(clk$id)
    leafletProxy("ps_map") %>% flyTo(clk$lng, clk$lat, zoom = 13)
  })

  output$ps_panel <- renderUI({
    s <- sel_station()
    if (is.null(s))   # default prompt before anything is clicked
      return(div(class = "ps-empty",
        svg_to_tag("pin", INDOT$navy, 26),
        tags$div(tags$b("Click a station on the map"),
          tags$div(class = "muted",
            "Its PlugShare detail and a link to the live listing will appear here."))))
    line <- function(k, v) if (!is.na(v) && v != "")
      div(class = "ps-line", span(class = "k", k), span(class = "v", v)) else NULL
    plugs <- if (!is.na(s$plugs) && s$plugs != "")
      lapply(strsplit(s$plugs, ",\\s*")[[1]], function(p) span(class = "ps-plug", p)) else NULL
    open <- if (s$plugshare_url != "")
      tags$a(class = "ps-open", href = s$plugshare_url, target = "_blank", rel = "noopener",
             HTML("Open full PlugShare listing &#8599;"))
      else span(class = "ps-note", "Not listed on PlugShare.")
    div(class = "ps-panel",
      div(class = "ps-head", tags$h3(s$station_name), div(class = "ps-sub", s$address),
          span(class = paste("ps-status", paste0("st-", s$cls)), s$disp)),
      div(class = "ps-body",
        if (!is.null(plugs)) div(style = "margin-bottom:8px", plugs),
        line("Network", s$network), line("Ports", s$chargers),
        line("PlugShare ID", s$location_id),
        line("Open date", if (!is.null(s$open_date)) s$open_date else ""),
        line("Verified", s$verified_date),
        line("Last live check", s$last_checked),
        if (!is.na(s$notes) && s$notes != "") div(class = "ps-note", s$notes),
        if (s$location_id != "")
          actionButton("check_one", "Check this station's live status",
                       icon = icon("satellite-dish"), class = "btn btn-gold btn-sm w-100 mt-2 mb-2"),
        open,
        div(class = "ps-note", "PlugShare blocks iframe embedding, so this recreates the listing; the link opens the live page.")))
  })
}

shinyApp(app_ui, server)
