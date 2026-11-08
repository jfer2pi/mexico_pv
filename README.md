# Mexico Solar PV Fleet

**[Open the live app](https://jfer2pi-pv-mexico.share.connect.posit.cloud/)** · Part of [Fernando Rodríguez Alvarado's portfolio](https://jfer2pi.github.io)

Shiny dashboard of utility-scale solar PV in Mexico, the sibling of the US Solar PV Fleet app.
One self-contained `app.R`, no API keys.

- **Bubble map** of every PV site on OpenStreetMap tiles, sized by MW. Operating sites are
  filled; pipeline sites (under construction, pre-construction, announced) are dashed rings.
  Shelved and cancelled projects are separate layers, off by default.
- **Capacity trajectory**, state and size breakdowns, KPI cards, owner table and a searchable
  site table that flies the map to a site.
- **CNE permits** (optional): permit number, holder and scheme joined onto sites, plus a table
  of every solar permit and the site it matched.

## Data

| Source | What it gives | How it gets in |
|---|---|---|
| Global Energy Monitor, Global Solar Power Tracker (CC BY 4.0) | Plant and phase list: status, MW, owner, start year, coordinates | **Required.** Download the .xlsx from the [GEM download page](https://globalenergymonitor.org/projects/global-solar-power-tracker/download-data/) (short form) and save it next to `app.R`. Any name matching `*solar*tracker*.xlsx`; the newest is used. |
| CNE (ex-CRE) generation permits | Permit number, holder, scheme, status, permitted MW, coordinates in the address | Optional. The monthly open-data export, e.g. [`ue_per_gen_hist_ago26.csv`](https://repodatos.atdt.gob.mx/api_update/cne/electricidad/ue_per_gen_hist_ago26.csv), saved next to `app.R` under its original name. Kept current by the monthly GitHub Action. |

The parsed data is cached to `mx_solar.rds` and rebuilt automatically when either file changes.

### Permit matching

Each solar permit goes to its best GEM site, in tiers:

- **exact**: GEM lists the CRE permit number for the site (`Other IDs`).
- **strong**: coordinates parsed from the permit address fall near the GEM point (with
  capacity or name support where GEM's location is approximate), or same state with a clear
  name overlap backed by municipality or capacity. State names don't count as name evidence.
- **possible**: same municipality and similar capacity only. Shown in the permits table but
  not attached to sites, since 30 MW small-producer projects often share a municipality.

Exact and strong matches are attached to sites. A site can carry several permits
(expansions, re-issues, LSPEE to LIE migrations); the most current one by status is shown.
Capacities above 1,500 are treated as kW, which a few rows evidently are.

## Staying up to date

`.github/workflows/check-data-releases.yml` runs `scripts/check_releases.R` on the 1st of
every month (or on demand from the Actions tab):

- **CNE permits, reminder.** The export is published at
  `https://repodatos.atdt.gob.mx/api_update/cne/electricidad/ue_per_gen_hist_<mes><yy>.csv`
  (e.g. `ago26`), but that server refuses GitHub's runners (HTTP 403). The check still
  tries each month and, if a download ever gets through, updates the export and pushes it.
  Otherwise, once the export in the repo is more than 3 months old, it opens an issue
  listing the newer export URLs: open them in a browser, save the newest that downloads
  next to `app.R` under its original name, delete the old one and push.
- **GEM tracker, notify only.** GEM's download sits behind a form (the file comes from a
  signed link that expires in 10 minutes), so the check compares the newest release month
  on GEM's pages with the `Global-Solar-Power-Tracker-<Month>-<Year>.xlsx` in the repo and
  opens an issue when there is a newer one. Download it, keep GEM's file name, delete the
  old one, regenerate the manifest and push.
- Either case opens a `data-release` issue (GitHub emails you). A source that can't be
  fetched or read fails the run, which GitHub also emails, so a broken check can't hide a
  release.

Keep both files under their original names: the check and the app read the release
month from them. State is kept on the `release-check-state` branch, so routine runs don't
redeploy `main`, and the monthly commit keeps GitHub from disabling the schedule.

## Run locally

```r
install.packages(c("shiny", "bslib", "readxl", "dplyr", "tidyr",
                   "leaflet", "plotly", "DT", "htmltools"))
shiny::runApp()
```

## Deploy on Posit Connect Cloud

In Connect Cloud choose **Publish → Shiny**, pick this repo and branch `main`, and set
`app.R` as the primary file.

`manifest.json` is maintained by `.github/workflows/write-manifest.yml`: any push to `main`
that changes `app.R`, the GEM .xlsx or the CNE export regenerates it on GitHub and commits it
back, so there's no need to run `rsconnect::writeManifest()` locally. To do it by hand:

```r
rsconnect::writeManifest(appFiles = c("app.R", list.files(
  pattern = "(solar.*tracker.*\\.xlsx|ue_per_gen_hist_.*\\.csv)$", ignore.case = TRUE)))
```

## Credits

Plants: Global Energy Monitor, Global Solar Power Tracker, CC BY 4.0.
Permits: Comisión Nacional de Energía (formerly Comisión Reguladora de Energía).
Map tiles © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors.
