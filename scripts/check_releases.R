#!/usr/bin/env Rscript
# =============================================================================
# Monthly check for new source data behind the Mexico Solar PV Fleet app.
#
#  - CNE generation permits: the open-data export is published at a stable URL
#    named by month (ue_per_gen_hist_<mes><yy>.csv, e.g. ago26 = August 2026).
#    Tries the last 12 months, newest first. When one is newer than the export
#    in the repo, downloads it to the repo root and removes the older export.
#    The workflow then regenerates manifest.json and pushes to main.
#  - GEM Global Solar Power Tracker: the download is behind a form, so this only
#    detects a release newer than the Global-Solar-Power-Tracker-<Month>-<Year>.xlsx
#    in the repo and asks you to download it. Keeps asking until you do.
#
# Writes:
#   release-check.md, release-check-title.txt  issue body and title
#   $STATE_FILE (default data-state.json)      last check, last GEM release seen
#   $GITHUB_OUTPUT notify=true|false, cne_updated=true|false
#   $GITHUB_STEP_SUMMARY
# Exits 1 when a source could not be fetched or read, so the run fails loudly.
# =============================================================================

suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

STATE_FILE <- Sys.getenv("STATE_FILE", "data-state.json")
UA         <- "Mozilla/5.0 (compatible; mx-solar-pv monthly release check)"
GEM_PAGES  <- c(
  "https://globalenergymonitor.org/projects/global-solar-power-tracker/",
  "https://globalenergymonitor.org/projects/global-solar-power-tracker/download-data/"
)
GEM_DL     <- GEM_PAGES[2]
CNE_URL    <- "https://repodatos.atdt.gob.mx/api_update/cne/electricidad/ue_per_gen_hist_%s.csv"
MONTHS     <- paste(month.name, collapse = "|")
MESES      <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
MESES_ALT  <- c(sep = "sept")   # alternative spellings to try

get_html <- function(u) {
  tryCatch(
    request(u) |> req_user_agent(UA) |> req_timeout(60) |> req_retry(max_tries = 3) |>
      req_perform() |> resp_body_string(),
    error = function(e) { message("Fetch failed: ", u, " | ", conditionMessage(e)); NA_character_ })
}

month_date <- function(mon, yr) {
  m <- match(tolower(mon), tolower(month.name))
  as.Date(ifelse(is.na(m), NA, sprintf("%s-%02d-01", yr, m)))
}
fmt_month <- function(d) if (length(d) != 1 || is.na(d)) "none" else format(d, "%B %Y")
first_of_month <- function(d) as.Date(format(d, "%Y-%m-01"))
add_months <- function(d, k) {
  d0 <- first_of_month(d)
  if (k == 0) d0 else seq(d0, by = paste(k, "months"), length.out = 2)[2]
}

# ---- CNE generation permits -------------------------------------------------

cne_tag   <- function(d) sprintf("%s%s", MESES[as.integer(format(d, "%m"))], format(d, "%y"))
cne_month <- function(f) {
  m <- regmatches(tolower(f), regexec("ue_per_gen_hist_([a-z]+)([0-9]{2})", tolower(f)))
  as.Date(vapply(m, function(x) {
    if (length(x) < 3) return(NA_character_)
    k <- match(x[2], c(MESES, unname(MESES_ALT)))
    if (is.na(k)) return(NA_character_)
    if (k > 12) k <- match(names(MESES_ALT)[k - 12], MESES)
    sprintf("20%s-%02d-01", x[3], k)
  }, ""))
}

repo_cne  <- list.files(".", pattern = "^ue_per_gen_hist_.*\\.csv$", ignore.case = TRUE)
cne_repo  <- if (length(repo_cne)) max(cne_month(repo_cne), na.rm = TRUE) else as.Date(NA)
if (length(cne_repo) && is.infinite(cne_repo)) cne_repo <- as.Date(NA)

# Newest month first; stop at the first export that exists
cne_site <- as.Date(NA); cne_site_url <- NA_character_; cne_tmp <- NA_character_
cne_errors <- 0L; cne_tried <- character()
for (k in 0:12) {
  d <- add_months(Sys.Date(), -k)
  if (!is.na(cne_repo) && d < cne_repo) break      # nothing older is of interest
  tags <- cne_tag(d)
  mes  <- MESES[as.integer(format(d, "%m"))]
  if (mes %in% names(MESES_ALT)) tags <- c(tags, sub(mes, MESES_ALT[[mes]], tags, fixed = TRUE))
  for (tg in tags) {
    url <- sprintf(CNE_URL, tg); tmp <- tempfile(fileext = ".csv")
    ok <- tryCatch({
      resp <- request(url) |> req_user_agent(UA) |> req_timeout(120) |>
        req_error(is_error = \(r) FALSE) |> req_perform(path = tmp)
      st <- resp_status(resp)
      if (st >= 500) cne_errors <- cne_errors + 1L
      sz <- if (file.exists(tmp)) file.size(tmp) else 0
      head1 <- if (sz > 0) readLines(tmp, n = 1, warn = FALSE, encoding = "UTF-8") else ""
      cne_tried <- c(cne_tried, sprintf("%s: HTTP %d, %s bytes, %s", tg, st,
                                        format(sz, big.mark = ","),
                                        resp_header(resp, "content-type") %||% "no content-type"))
      st == 200 && sz > 10000 && grepl("permisionario", head1, ignore.case = TRUE)
    }, error = function(e) {
      cne_errors <<- cne_errors + 1L
      cne_tried <<- c(cne_tried, sprintf("%s: %s", tg, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) { cne_site <- d; cne_site_url <- url; cne_tmp <- tmp; break }
  }
  if (!is.na(cne_site)) break
}

message("CNE URLs tried:\n  ", paste(cne_tried, collapse = "\n  "))
# Not finding even the month already in the repo means the check itself is broken
cne_problem <- if (is.na(cne_site))
  sprintf("no CNE export could be downloaded (tried: %s)", paste(cne_tried, collapse = "; ")) else NULL
cne_new <- !is.na(cne_site) && (is.na(cne_repo) || cne_site > cne_repo)
cne_file <- NA_character_
if (cne_new) {
  cne_file <- basename(sub("\\?.*$", "", cne_site_url))
  file.copy(cne_tmp, cne_file, overwrite = TRUE)
  old <- setdiff(repo_cne, cne_file)
  if (length(old)) file.remove(old)
  message("CNE: downloaded ", cne_file, if (length(old)) paste(", removed", paste(old, collapse = ", ")))
}

# ---- GEM -------------------------------------------------------------------

# Release dates mentioned on GEM's pages, only in release-specific contexts so
# news items dated this month are not mistaken for a data release
gem_release_dates <- function(html) {
  if (is.na(html)) return(as.Date(character()))
  out <- as.Date(character())
  m <- regmatches(html, gregexpr("Official-data-download/(20[0-9]{2})-([0-9]{2})", html))[[1]]
  if (length(m)) out <- c(out, as.Date(paste0(sub(".*/", "", m), "-01")))
  pats <- c(sprintf("Tracker[- ](%s)[- ](20[0-9]{2})", MONTHS),
            sprintf("(%s)[ -](20[0-9]{2})[ -][Rr]elease", MONTHS),
            sprintf("[Rr]elease[^<]{0,40}?(%s)[ -](20[0-9]{2})", MONTHS))
  for (p in pats) {
    for (h in regmatches(html, gregexpr(p, html, perl = TRUE))[[1]]) {
      out <- c(out, month_date(regmatches(h, regexpr(MONTHS, h)),
                               regmatches(h, regexpr("20[0-9]{2}", h))))
    }
  }
  out[!is.na(out)]
}

repo_gem  <- list.files(".", pattern = "solar.*tracker.*\\.xlsx$", ignore.case = TRUE)
gem_dates <- do.call(c, c(list(as.Date(NA)), lapply(repo_gem, function(f) {
  h <- regmatches(f, regexpr(sprintf("(%s)[- _]?(20[0-9]{2})", MONTHS), f))
  if (!length(h)) return(as.Date(NA))
  month_date(regmatches(h, regexpr(MONTHS, h)), regmatches(h, regexpr("20[0-9]{2}", h)))
})))
gem_repo <- if (any(!is.na(gem_dates))) max(gem_dates, na.rm = TRUE) else as.Date(NA)

gem_html <- vapply(GEM_PAGES, get_html, "")
gem_seen <- gem_release_dates(paste(gem_html[!is.na(gem_html)], collapse = "\n"))
gem_site <- if (length(gem_seen)) max(gem_seen) else as.Date(NA)

gem_problem <- if (all(is.na(gem_html))) "could not fetch GEM's tracker pages" else
               if (is.na(gem_site)) "fetched GEM's pages but found no release month (page layout may have changed)" else NULL
gem_new <- is.null(gem_problem) && (is.na(gem_repo) || gem_site > gem_repo)

# ---- Report ----------------------------------------------------------------

lines <- unlist(c(
  "Monthly data check for the Mexico Solar PV Fleet app.", "",
  "### CNE generation permits",
  sprintf("- Was in repo: **%s**%s", fmt_month(cne_repo),
          if (length(repo_cne)) sprintf(" (`%s`)", paste(repo_cne, collapse = "`, `")) else ""),
  sprintf("- Latest on the CNE open-data server: **%s**", fmt_month(cne_site)),
  if (!is.null(cne_problem)) sprintf("- Problem: %s.", cne_problem),
  if (cne_new) c("", sprintf("**Downloaded `%s` and pushed it to `main`** with a regenerated manifest. Connect Cloud redeploys and the app rebuilds its cache and permit matches on the next load.", cne_file)),
  "", "### GEM Global Solar Power Tracker",
  sprintf("- In repo: **%s**%s", fmt_month(gem_repo),
          if (length(repo_gem)) sprintf(" (`%s`)", paste(repo_gem, collapse = "`, `")) else ""),
  sprintf("- On GEM's site: **%s**", fmt_month(gem_site)),
  if (!is.null(gem_problem)) sprintf("- Problem: %s.", gem_problem),
  if (gem_new) c("", sprintf("**New release, download needed.** GEM's download is behind a form: get the .xlsx from the [GEM download page](%s), keep GEM's file name, put it next to `app.R`, delete the old one, regenerate the manifest and push.", GEM_DL))
))

parts <- c(if (gem_new) sprintf("GEM solar tracker %s (download needed)", fmt_month(gem_site)),
           if (cne_new) sprintf("CNE permits %s (updated automatically)", fmt_month(cne_site)))
notify <- length(parts) > 0
title  <- if (notify) paste("New data:", paste(parts, collapse = " + ")) else "No new data"

writeLines(lines, "release-check.md")
writeLines(title, "release-check-title.txt")
cat(paste(c(paste("##", title), lines), collapse = "\n"), "\n")

if (nzchar(Sys.getenv("GITHUB_STEP_SUMMARY")))
  write(c(paste("##", title), lines), Sys.getenv("GITHUB_STEP_SUMMARY"), append = TRUE)
if (nzchar(Sys.getenv("GITHUB_OUTPUT")))
  write(c(sprintf("notify=%s", tolower(notify)), sprintf("cne_updated=%s", tolower(cne_new))),
        Sys.getenv("GITHUB_OUTPUT"), append = TRUE)

state <- if (file.exists(STATE_FILE)) tryCatch(fromJSON(STATE_FILE), error = \(e) list()) else list()
state$last_checked    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
state$gem_latest_site <- if (is.na(gem_site)) state$gem_latest_site else format(gem_site)
state$cne_latest_site <- if (is.na(cne_site)) state$cne_latest_site else format(cne_site)
state$cne_links <- NULL
dir.create(dirname(STATE_FILE), showWarnings = FALSE, recursive = TRUE)
write_json(state, STATE_FILE, auto_unbox = TRUE, pretty = TRUE)

if (!is.null(gem_problem) || !is.null(cne_problem)) quit(status = 1)
