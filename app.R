# =============================================================================
# Mexico Solar PV Fleet: operating plants and development pipeline
# -----------------------------------------------------------------------------
# One self-contained file. No API keys anywhere.
#
# Data (read from files in this folder, cached to mx_solar.rds):
#   1. Global Energy Monitor, Global Solar Power Tracker (CC BY 4.0). Required.
#      Phase-level status, MW, owner, start year, coordinates.
#      GEM gates the download behind a short form, so it cannot be fetched
#      automatically. Download the .xlsx once from
#      https://globalenergymonitor.org/projects/global-solar-power-tracker/download-data/
#      and drop it in this folder (any file name matching *solar*tracker*.xlsx).
#      The newest such file is used; replacing it triggers a rebuild.
#   2. CNE (ex-CRE) generation permits export. Optional.
#      https://repodatos.atdt.gob.mx/api_update/cne/electricidad/ue_per_gen_hist_<mes><yy>.csv
#      Drop it in this folder (keep the ue_per_gen_hist name, or any name
#      containing "permis"). Solar permits are matched to GEM sites in tiers:
#      permit number listed by GEM, coordinates parsed from the permit address,
#      then name with municipality or capacity. Exact and strong matches add
#      permit number, holder, scheme and status to the site.
#
# Map:   OpenStreetMap standard tiles (dark-filtered + standard). Light use
#        only, per https://operations.osmfoundation.org/policies/tiles/
# Theme: Linear-style dark UI, matching the US Solar PV Fleet app.
#
# install.packages(c("shiny","bslib","readxl","dplyr","tidyr",
#                    "leaflet","plotly","DT","htmltools"))
# shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# ---- Config -----------------------------------------------------------------

CACHE_FILE     <- "mx_solar.rds"
CACHE_VERSION  <- 3L   # bump when the cached structure changes
GEM_PATTERN    <- "solar.*tracker.*\\.xlsx$"
PERMIT_PATTERN <- "(permis|per_gen).*\\.(xlsx|xls|csv)$"
GEM_URL        <- "https://globalenergymonitor.org/projects/global-solar-power-tracker/download-data/"
CNE_URL        <- "https://www.gob.mx/cne/documentos/estadisticas-e-informacion-en-materia-de-permisos-de-electricidad"
# Monthly CNE open-data export, e.g. ue_per_gen_hist_ago26.csv (August 2026)
CNE_DATA_URL   <- "https://repodatos.atdt.gob.mx/api_update/cne/electricidad/ue_per_gen_hist_%s%s.csv"
MESES          <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6, jul = 7,
                    ago = 8, sep = 9, sept = 9, oct = 10, nov = 11, dic = 12)

STAGES <- c("Operating", "Under construction", "Pre-construction",
            "Announced", "Shelved", "Cancelled")
STAGE_COLS <- c(
  "Operating"          = "#5E6AD2",  # Linear indigo
  "Under construction" = "#F2994A",  # orange
  "Pre-construction"   = "#F2C94C",  # yellow
  "Announced"          = "#B59AF8",  # lavender
  "Shelved"            = "#8A8F98",  # grey
  "Cancelled"          = "#EB5757"   # red
)
PIPELINE       <- c("Under construction", "Pre-construction", "Announced")
STALLED        <- c("Shelved", "Cancelled")
DEFAULT_STAGES <- c("Operating", "Under construction", "Pre-construction")
CY             <- as.integer(format(Sys.Date(), "%Y"))

# ---- Small helpers ----------------------------------------------------------

clean_names <- function(x) {
  x <- iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

norm_txt <- function(x) {
  x <- iconv(tolower(as.character(x)), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}

norm_state <- function(x) {
  s <- sub("^estado de ", "", norm_txt(x))
  s[s %in% c("cdmx", "ciudad de mexico", "distrito federal", "df", "mexico city")] <- "ciudad de mexico"
  s[s %in% c("edomex", "state of mexico", "mexico state")] <- "mexico"
  s <- sub(" de ignacio de la llave$", "", s)   # Veracruz
  s <- sub(" de ocampo$", "", s)                # Michoacan
  s <- sub(" de zaragoza$", "", s)              # Coahuila
  s
}

# Numbers written either 1,234.5 or 1234,5
num_mx <- function(x) {
  x <- gsub("[^0-9,.-]", "", as.character(x))
  both <- grepl(",", x) & grepl("\\.", x)
  x[both] <- gsub(",", "", x[both])
  one <- grepl(",", x) & !both
  x[one] <- ifelse(grepl(",[0-9]{3}$", x[one]), gsub(",", "", x[one]), sub(",", ".", x[one]))
  suppressWarnings(as.numeric(x))
}

# First column whose cleaned name matches any of the regexes, in order
pickr <- function(d, ...) {
  for (p in c(...)) {
    nm <- grep(p, names(d), value = TRUE, perl = TRUE)
    if (length(nm)) return(as.character(d[[nm[1]]]))
  }
  rep(NA_character_, nrow(d))
}

na_dash <- function(x) ifelse(is.na(x) | !nzchar(trimws(x)), "-", x)

hav_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  12742 * asin(pmin(1, sqrt(a)))
}

# Month of a CNE export from its file name (ue_per_gen_hist_ago26 -> 2026-08-01)
cne_file_month <- function(f) {
  m <- regmatches(tolower(basename(f)), regexec("per_gen_hist_([a-z]+)([0-9]{2})", tolower(basename(f))))
  vapply(m, function(x) {
    if (length(x) < 3 || is.na(MESES[x[2]])) return(NA_real_)
    as.numeric(as.Date(sprintf("20%s-%02d-01", x[3], MESES[[x[2]]])))
  }, 0)
}

# Prefer the newest CNE export by the month in its name, else the newest file
find_permit_file <- function() {
  f <- list.files(".", pattern = PERMIT_PATTERN, ignore.case = TRUE, full.names = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]
  if (!length(f)) return(NULL)
  mo <- cne_file_month(f)
  if (any(!is.na(mo))) return(f[which.max(mo)])
  f[which.max(file.mtime(f))]
}

# Coordinates written into CNE permit addresses. Formats vary: degrees and
# decimal minutes, full DMS, "O" / "0" / "Oeste" / nothing / even "E" for west,
# occasional minus signs. Hemisphere letters are ignored: the first degree value
# between 14 and 33 is the latitude, the next one between 86 and 118 the
# (western) longitude.
COORD_RX <- paste0("(-?[0-9]{1,3})\\s*°\\s*([0-9]{1,3}(?:[.,][0-9]+)?)\\s*",
                   "[´’'′`]?\\s*(?:([0-9]{1,3}(?:[.,][0-9]+)?)\\s*",
                   "(?:\"|”|″|''|’’))?")
parse_coords <- function(txt) {
  txt <- ifelse(is.na(txt), "", gsub("\\s+", " ", txt))
  hits <- regmatches(txt, gregexpr(COORD_RX, txt, perl = TRUE))
  out <- t(vapply(hits, function(h) {
    if (!length(h)) return(c(NA_real_, NA_real_))
    vals <- vapply(h, function(s) {
      p  <- regmatches(s, regexec(COORD_RX, s, perl = TRUE))[[1]]
      d  <- abs(as.numeric(p[2]))
      mi <- sub(",", ".", p[3]); se <- p[4]
      if (!grepl("\\.", mi) && nchar(mi) > 2) mi <- paste0(substr(mi, 1, 2), ".", substring(mi, 3))
      mi <- as.numeric(mi)
      sec <- if (!is.na(se) && nzchar(se)) as.numeric(sub(",", ".", se)) else 0
      if (sec >= 60) {   # "12'635"" means 12.635 minutes
        mi <- mi + sec / 10^nchar(sub("\\..*", "", se)); sec <- 0
      }
      d + mi / 60 + sec / 3600
    }, 0, USE.NAMES = FALSE)
    for (k in which(vals >= 14 & vals <= 33)) {
      w <- vals[seq_along(vals) > k]
      w <- w[w >= 86 & w <= 118]
      if (length(w)) return(c(vals[k], -w[1]))
    }
    c(NA_real_, NA_real_)
  }, numeric(2)))
  colnames(out) <- c("lat", "lon")
  out
}

# Municipality equality, tolerant of "Ascension" vs "Ejido Cuauhtemoc, Ascension"
mun_hit <- function(a, ms) {
  if (!nzchar(a) || !length(ms)) return(FALSE)
  any(ms == a | (nchar(ms) > 4 & nchar(a) > 4 &
                 (vapply(ms, \(m) grepl(m, a, fixed = TRUE), TRUE) |
                  vapply(ms, \(m) grepl(a, m, fixed = TRUE), TRUE))))
}

find_file <- function(pattern) {
  f <- list.files(".", pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]          # skip Excel lock files
  if (!length(f)) return(NULL)
  f[which.max(file.mtime(f))]
}

file_sig <- function(p) if (is.null(p)) "" else paste(basename(p), file.mtime(p), file.size(p))

# Name tokens for fuzzy matching, without legal forms and generic words
STOPWORDS <- c("s", "a", "de", "c", "v", "sa", "cv", "sapi", "rl", "srl", "del", "la", "el",
               "los", "las", "y", "the", "of", "and", "solar", "solares", "fotovoltaica",
               "fotovoltaico", "fotovoltaicos", "photovoltaic", "pv", "fv", "parque", "planta",
               "central", "granja", "farm", "park", "plant", "project", "proyecto", "energia",
               "energias", "energy", "renovable", "renovables", "renewable", "renewables",
               "power", "generacion", "generadora", "mexico", "mx", "mex", "i", "ii", "iii",
               "iv", "v", "phase", "fase", "unit", "100", "sun")
toks <- function(x) lapply(strsplit(norm_txt(x), " ", fixed = TRUE),
                           \(t) setdiff(unique(t[nchar(t) > 1]), STOPWORDS))
jac <- function(a, b) if (!length(a) || !length(b)) 0 else
  length(intersect(a, b)) / length(union(a, b))

# ---- 1. GEM Global Solar Power Tracker --------------------------------------

stage_from_gem <- function(s) {
  s <- trimws(tolower(s))
  out <- case_when(
    s == "operating"                             ~ "Operating",
    grepl("construction", s) & !grepl("^pre", s) ~ "Under construction",
    grepl("^pre", s)                             ~ "Pre-construction",
    s == "announced"                             ~ "Announced",
    grepl("^shelved|^mothballed", s)             ~ "Shelved",
    grepl("^cancel", s)                          ~ "Cancelled",
    TRUE                                         ~ NA_character_     # retired etc.
  )
  factor(out, levels = STAGES)
}

# The workbook has one data sheet per size band (e.g. "20 MW+", "1-20 MW") plus
# About sheets. Read every sheet that has a Latitude header.
read_gem <- function(path) {
  rows <- lapply(excel_sheets(path), function(sh) {
    head20 <- suppressMessages(read_excel(path, sheet = sh, col_names = FALSE, n_max = 20,
                                          col_types = "text", .name_repair = "minimal"))
    if (!nrow(head20)) return(NULL)
    hdr <- which(apply(head20, 1, \(r) any(tolower(trimws(r)) %in% "latitude")))[1]
    if (is.na(hdr)) return(NULL)
    d <- suppressMessages(read_excel(path, sheet = sh, skip = hdr - 1, col_types = "text",
                                     .name_repair = "minimal"))
    names(d) <- make.unique(clean_names(names(d)))
    d
  })
  d <- bind_rows(rows)
  if (!nrow(d)) stop("No data sheet with a Latitude column in ", basename(path), call. = FALSE)
  d <- d[norm_txt(pickr(d, "^country")) == "mexico", ]

  g <- tibble(
    loc_id       = pickr(d, "gem_location_id", "location_id"),
    phase_id     = pickr(d, "gem_phase_id", "phase_id"),
    plant        = pickr(d, "^project_name$", "^project_name"),
    phase        = pickr(d, "^phase_name"),
    owner        = pickr(d, "^owner_s$", "^owner$", "^owner"),
    operator     = pickr(d, "^operator"),
    local_name   = pickr(d, "^project_name_in_local"),
    other_names  = pickr(d, "^other_name"),
    other_ids    = pickr(d, "^other_ids_unit", "^other_ids"),
    state        = pickr(d, "^state_province", "^subnational"),
    city         = pickr(d, "^city"),
    local_area   = pickr(d, "^local_area"),
    major_area   = pickr(d, "^major_area"),
    status       = tolower(trimws(pickr(d, "^status"))),
    tech         = pickr(d, "^technology"),
    mw           = num_mx(pickr(d, "^capacity_mw", "^capacity$", "^capacity(?!_rating)")),
    rating       = pickr(d, "^capacity_rating"),
    year         = as.integer(num_mx(pickr(d, "^start_year"))),
    lat          = num_mx(pickr(d, "^latitude")),
    lon          = num_mx(pickr(d, "^longitude")),
    accuracy     = tolower(trimws(pickr(d, "^location_accuracy"))),
    wiki         = pickr(d, "wiki")
  ) |>
    mutate(stage = stage_from_gem(status),
           municipality = coalesce(major_area, local_area, city),
           owner = coalesce(na_if(trimws(owner), ""), na_if(trimws(operator), ""), "Unknown"),
           loc_id = coalesce(loc_id, paste(plant, round(lat, 3), round(lon, 3))),
           accuracy = coalesce(na_if(accuracy, ""), "approximate")) |>
    filter(!is.na(stage), !grepl("thermal", tolower(coalesce(tech, ""))),
           !is.na(mw), mw > 0, !is.na(lat), !is.na(lon))
  if (!nrow(g)) stop("GEM file has no Mexican PV rows: ", basename(path), call. = FALSE)
  g
}

# One row per GEM site (all phases), the unit for permit matching
site_table <- function(g) {
  g |> group_by(loc_id) |>
    summarise(plant = first(plant), local_name = first(local_name),
              other_names = paste(unique(na.omit(other_names)), collapse = " "),
              other_ids = paste(unique(na.omit(other_ids)), collapse = ","),
              owner = first(owner), operator = first(operator), state = first(state),
              city = first(city), local_area = first(local_area), major_area = first(major_area),
              lat = first(lat), lon = first(lon), accuracy = first(accuracy),
              mw = sum(mw), .groups = "drop")
}

# ---- 2. CNE generation permits (optional) -----------------------------------
#
# The CNE open-data export (ue_per_gen_hist_<mes><yy>.csv) lists every generation
# permit since the 1960s: permisionario, legal, modalidad, fecha_otorgamiento,
# num_per, capacidad_autorizada, generacion_estimada, inversion, fecha_operacion,
# energetico, act_eco, tecnologia, estatus, fecha_mod, entidad, municipio,
# direccion, pais_origen, fecha_termino. Column matching is by pattern, so
# renamed or reordered columns in later exports still read.

find_hdr <- function(raw, must) {
  n <- min(30, nrow(raw))
  if (!n) return(NA_integer_)
  which(vapply(seq_len(n), function(i) {
    r <- norm_txt(unlist(raw[i, ]))
    any(grepl(must, r)) && sum(nzchar(r)) >= 4
  }, logical(1)))[1]
}

SCHEME_LABELS <- c(
  "PP" = "Pequeña producción", "GEN" = "Generación (LIE)", "MEM" = "Mercado eléctrico (LSE)",
  "AUT" = "Autoabastecimiento", "AUTC" = "Autoconsumo", "ABAI" = "Abasto aislado",
  "ABANI" = "Abasto aislado (no interconectado)", "COG" = "Cogeneración",
  "PIE" = "Producción independiente", "EXP" = "Exportación", "UPC" = "Usos propios continuos")
scheme_label <- function(x) {
  k <- toupper(gsub("[^A-Za-z]", "", x))
  k <- sub("^(AUTC|ABANI|ABAI|AUT|GEN|PP|MEM|COG|PIE|EXP|UPC).*$", "\\1", k)
  out <- unname(SCHEME_LABELS[k])
  ifelse(is.na(out), trimws(x), out)
}

# Most current permit first when a site has several (re-issues, migrations)
STATUS_RANK <- c("en operacion" = 1, "en operacion parcial" = 2, "en construccion" = 3,
                 "por iniciar obras" = 4, "migrado" = 5, "terminado" = 6, "caducado" = 7,
                 "revocado" = 8)
status_rank <- function(x) { r <- unname(STATUS_RANK[norm_txt(x)]); ifelse(is.na(r), 9, r) }

read_permits <- function(path) {
  ext <- tolower(tools::file_ext(path))
  tables <- if (ext == "csv") {
    raw <- tryCatch(read.csv(path, header = FALSE, colClasses = "character",
                             fileEncoding = "UTF-8-BOM", check.names = FALSE),
                    error = \(e) read.csv(path, header = FALSE, colClasses = "character",
                                          fileEncoding = "latin1", check.names = FALSE))
    list(as_tibble(raw, .name_repair = "minimal"))
  } else {
    lapply(excel_sheets(path), \(sh) suppressMessages(
      read_excel(path, sheet = sh, col_names = FALSE, col_types = "text",
                 .name_repair = "minimal")))
  }
  for (raw in tables) {
    hdr <- find_hdr(raw, "permis")
    if (is.na(hdr)) next
    d <- raw[-seq_len(hdr), ]
    names(d) <- make.unique(clean_names(unlist(raw[hdr, ])))

    energy <- norm_txt(pickr(d, "^energetico", "energ", "combustible", "fuente"))
    tech   <- norm_txt(pickr(d, "^tecnologia", "tecnolog"))
    solar  <- grepl("\\bsol\\b|solar|fotovolt", energy) | grepl("fotovolt|solar", tech)
    if (all(!nzchar(energy) & !nzchar(tech)))      # no technology columns: search whole rows
      solar <- grepl("solar|fotovolt", norm_txt(apply(d, 1, paste, collapse = " ")))
    d <- d[solar, ]
    if (!nrow(d)) next

    holder  <- trimws(pickr(d, "^permisionario", "razon_social", "titular", "^nombre"))
    address <- pickr(d, "^direccion", "domicilio", "ubicaci")
    mw_raw  <- num_mx(pickr(d, "^capacidad_autorizada", "capacidad.*mw", "capacidad"))
    xy      <- parse_coords(address)
    P <- tibble(
      permit_no    = toupper(trimws(pickr(d, "^num_per$", "^num.*per", "^no_.*permiso",
                                         "^permiso$", "permiso(?!nario)"))),
      holder       = holder,
      # Holder names sometimes carry the plant: "Diez Solar, S. A. P. I. de C. V., Central Zacatecas"
      plant_name   = ifelse(grepl(",\\s*(Central|Planta)\\s+", holder),
                            sub("^.*,\\s*(Central|Planta)\\s+", "", holder), NA_character_),
      legal        = pickr(d, "^legal$", "^ley"),
      scheme_code  = trimws(pickr(d, "^modalidad", "esquema", "figura")),
      status       = trimws(pickr(d, "^estatus", "^status", "situaci")),
      granted      = substr(pickr(d, "fecha_otorg", "otorgamiento"), 1, 10),
      in_service   = substr(pickr(d, "^fecha_operacion", "operacion"), 1, 10),
      # A few rows are evidently in kW (e.g. 82,856 for an ~83 MW plant)
      mw           = ifelse(!is.na(mw_raw) & mw_raw > 1500, mw_raw / 1000, mw_raw),
      mw_in_kw     = !is.na(mw_raw) & mw_raw > 1500,
      investment   = num_mx(pickr(d, "inversi")),
      state        = trimws(pickr(d, "^entidad", "^estado$")),
      municipality = trimws(pickr(d, "^municipio")),
      address      = address,
      lat          = xy[, "lat"],
      lon          = xy[, "lon"]
    ) |>
      mutate(scheme = scheme_label(scheme_code),
             in_service = ifelse(substr(in_service, 1, 4) == "1900", NA, in_service)) |>
      filter(!is.na(permit_no) | !is.na(holder))
    return(P)
  }
  warning("No permit table with solar rows found in ", basename(path), call. = FALSE)
  NULL
}

# State names are not evidence of a match ("Central Jalisco" vs "Jalisco solar farm")
STATE_TOKENS <- unique(unlist(toks(c(
  "Aguascalientes", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Coahuila",
  "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacan",
  "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", "Quintana Roo",
  "San Luis Potosi", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
  "Yucatan", "Zacatecas", "Ciudad"))))
name_toks <- function(x) lapply(toks(x), \(t) setdiff(t, STATE_TOKENS))

# Each permit goes to its best GEM site; several permits can share one site.
#   exact    : GEM lists the CRE permit number for the site
#   strong   : permit coordinates near the GEM point (with capacity or name
#              support when GEM's point is approximate), or same state with a
#              clear name overlap (municipality or capacity backing it up)
#   possible : same state and municipality with a matching capacity only.
#              Shown in the permits table, not attached to sites: 30 MW is the
#              standard small-producer size, so this alone is ambiguous.
match_permits <- function(P, st) {
  ids <- regmatches(st$other_ids, gregexpr("CRE:\\s*[A-Za-z0-9/]+", st$other_ids))
  id_tab <- tibble(j = rep(seq_len(nrow(st)), lengths(ids)),
                   pno = toupper(trimws(sub("CRE:\\s*", "", unlist(ids)))))

  s_st  <- norm_state(st$state); p_st <- norm_state(P$state)
  s_mun <- lapply(seq_len(nrow(st)), \(j) {
    m <- sub("^municipio de ", "", norm_txt(c(st$city[j], st$local_area[j], st$major_area[j])))
    unique(m[nzchar(m)])
  })
  p_mun <- sub("^municipio de ", "", norm_txt(P$municipality))
  s_all <- name_toks(paste(st$plant, st$local_name, st$other_names, st$owner, st$operator))
  s_nm  <- name_toks(paste(st$plant, st$local_name, st$other_names))
  p_tk  <- name_toks(P$holder)
  p_cn  <- name_toks(ifelse(is.na(P$plant_name), "", P$plant_name))
  exact_pt <- st$accuracy == "exact"

  res <- lapply(seq_len(nrow(P)), function(i) {
    j_id <- id_tab$j[id_tab$pno == P$permit_no[i]]
    if (length(j_id)) return(list(j = j_id[1], conf = "exact", how = "permit no. listed by GEM",
                                  score = 10))
    ratio <- P$mw[i] / st$mw
    cap   <- !is.na(ratio) & ratio >= 0.6 & ratio <= 1.67
    loose <- !is.na(ratio) & ratio >= 0.4 & ratio <= 2.5
    nm <- vapply(s_all, \(t) jac(p_tk[[i]], t), 0)
    if (length(p_cn[[i]])) nm <- pmax(nm, vapply(s_nm, \(t) jac(p_cn[[i]], t), 0))
    same_st <- nzchar(p_st[i]) & s_st == p_st[i]
    mun <- same_st & vapply(s_mun, \(m) mun_hit(p_mun[i], m), TRUE)

    score <- numeric(nrow(st)); how <- character(nrow(st))
    upd <- function(ok, val, label) {
      val <- rep_len(val, length(score)); label <- rep_len(label, length(score))
      better <- ok & val > score
      better[is.na(better)] <- FALSE
      score[better] <<- val[better]; how[better] <<- label[better]
    }
    if (!is.na(P$lat[i])) {
      d   <- hav_km(P$lat[i], P$lon[i], st$lat, st$lon)
      lim <- ifelse(exact_pt, 3, 12)
      ok  <- d <= lim & ((exact_pt & d <= 1.5) | cap | nm >= 0.2)
      upd(ok, 3 + (1 - d / lim), sprintf("coordinates (%.1f km)", d))
    }
    upd(mun & nm >= 0.2, 2.5 + nm + 0.25 * cap, "municipality + name")
    upd(same_st & nm >= 0.5 & (loose | nm >= 0.8), 2 + nm, "name")
    upd(same_st & nm >= 0.34 & cap, 1.9 + nm, "name + capacity")
    upd(mun & cap & score == 0, 1 + 0.5 * (1 - abs(log(ratio))), "municipality + capacity")

    if (!any(score > 0)) return(NULL)
    j <- which.max(score)
    list(j = j, conf = if (score[j] >= 1.9) "strong" else "possible", how = how[j],
         score = round(score[j], 2))
  })

  hit <- !vapply(res, is.null, TRUE)
  P$match_conf <- "none"; P$match_how <- NA_character_; P$match_score <- NA_real_
  P$matched_site <- NA_character_; P$loc_id <- NA_character_
  if (any(hit)) {
    j <- vapply(res[hit], `[[`, 0, "j")
    P$match_conf[hit]   <- vapply(res[hit], `[[`, "", "conf")
    P$match_how[hit]    <- vapply(res[hit], `[[`, "", "how")
    P$match_score[hit]  <- vapply(res[hit], `[[`, 0, "score")
    P$matched_site[hit] <- st$plant[j]
    P$loc_id[hit]       <- st$loc_id[j]
  }

  # Attach exact + strong matches to sites; the most current permit leads
  by_site <- P |>
    filter(match_conf %in% c("exact", "strong")) |>
    mutate(rank = status_rank(status)) |>
    arrange(loc_id, rank, desc(granted)) |>
    group_by(loc_id) |>
    summarise(permit_no = first(permit_no), permit_holder = first(holder),
              scheme = first(scheme), permit_status = first(status),
              permit_mw = first(mw), permit_conf = first(match_conf),
              n_permits = n(),
              permit_list = paste(sprintf("%s (%s)", permit_no, status), collapse = "; "),
              .groups = "drop")
  list(by_site = by_site, permits = P)
}

# ---- Build + cache ----------------------------------------------------------

build_data <- function() {
  gem_path <- find_file(GEM_PATTERN)
  per_path <- find_permit_file()
  if (is.null(gem_path))
    stop("No GEM Global Solar Power Tracker file in the app folder. Download the .xlsx from ",
         GEM_URL, " and save it next to app.R (name matching *solar*tracker*.xlsx).",
         call. = FALSE)

  g <- read_gem(gem_path)

  permits <- NULL
  P <- if (!is.null(per_path)) tryCatch(read_permits(per_path), error = function(e) {
    warning("Permit file not read: ", conditionMessage(e), call. = FALSE); NULL }) else NULL
  if (!is.null(P) && nrow(P)) {
    m <- match_permits(P, site_table(g))
    g <- left_join(g, m$by_site, by = "loc_id")
    permits <- m$permits
  }
  for (cl in c("permit_no", "permit_holder", "scheme", "permit_status", "permit_conf", "permit_list"))
    if (is.null(g[[cl]])) g[[cl]] <- NA_character_
  if (is.null(g$permit_mw)) g$permit_mw <- NA_real_
  g$n_permits <- if (is.null(g$n_permits)) 0L else coalesce(g$n_permits, 0L)

  list(version = CACHE_VERSION, gens = g, permits = permits,
       gem_file = basename(gem_path), gem_sig = file_sig(gem_path),
       permit_file = if (is.null(per_path)) NA_character_ else basename(per_path),
       permit_sig = file_sig(per_path),
       built = Sys.time())
}

# Rebuild only when the GEM or permits file in the folder has changed
load_data <- function() {
  cached <- if (file.exists(CACHE_FILE)) tryCatch(readRDS(CACHE_FILE), error = \(e) NULL)
  if (!is.null(cached) && identical(cached$version, CACHE_VERSION) &&
      identical(file_sig(find_file(GEM_PATTERN)), cached$gem_sig) &&
      identical(file_sig(find_permit_file()), cached$permit_sig)) return(cached)
  out <- build_data()
  saveRDS(out, CACHE_FILE)
  out
}

# ---- Snapshot prep (everything the UI derives from one build) ---------------

prep_data <- function(s) {
  # Pipeline phases whose expected start year has already passed show as this year
  G <- s$gens |>
    mutate(year_plot = ifelse(stage %in% PIPELINE & !is.na(year) & year < CY, CY, year))
  yrs <- G$year_plot[!is.na(G$year_plot)]
  site_mw <- G |> group_by(loc_id, stage) |> summarise(mw = sum(mw), .groups = "drop") |> pull(mw)
  list(
    raw      = s,
    G        = G,
    yr_rng   = if (length(yrs)) range(yrs) else c(CY - 10, CY + 3),
    total_op = sum(G$mw[G$stage == "Operating"]),
    site_max = max(1, ceiling(max(c(site_mw, 1)))),
    states   = sort(unique(na.omit(G$state))),
    schemes  = sort(unique(na.omit(G$scheme)))
  )
}

current_data <- local({
  key <- NULL; val <- NULL
  function() {
    s <- load_data()
    k <- format(s$built, "%Y-%m-%d %H:%M:%OS3")
    if (!identical(k, key)) { val <<- prep_data(s); key <<- k }
    val
  }
})
invisible(current_data())  # fail fast at startup with a clear message if no GEM file

# ---- Linear design tokens ---------------------------------------------------

lin <- list(
  bg = "#08090A", surface = "#0F1011", raised = "#141516",
  border = "rgba(255,255,255,0.08)", text = "#F7F8F8",
  muted = "#8A8F98", faint = "#62666D", accent = "#5E6AD2"
)

SIZE_BREAKS <- c(0, 5, 20, 100, 250, Inf)
SIZE_LABELS <- c("< 5 MW", "5 to 20", "20 to 100", "100 to 250", "> 250 MW")

bubble_r <- function(mw) pmin(pmax(sqrt(mw) * 0.95, 2.5), 34)

theme <- bs_theme(
  version = 5, bg = lin$bg, fg = lin$text, primary = lin$accent,
  secondary = lin$raised,
  base_font    = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  font_scale = 0.88,
  `border-radius` = "6px", `border-color` = lin$border,
  `card-bg` = lin$surface, `card-border-color` = lin$border,
  `input-bg` = lin$raised, `input-border-color` = lin$border
) |>
  bs_add_rules(sprintf("
  body { font-feature-settings:'cv11','ss01','tnum'; letter-spacing:-0.005em; }
  .navbar { background:%1$s !important; border-bottom:1px solid %4$s; }
  .brand { display:flex; align-items:center; gap:10px; font-weight:600; font-size:0.95rem; }
  .brand .logo { width:18px; height:18px; border-radius:5px;
                 background:linear-gradient(135deg,%5$s 0%%,#A5ABF2 100%%); }
  .brand .tag { color:%2$s; font-weight:400; font-size:0.78rem;
                border:1px solid %4$s; border-radius:999px; padding:1px 8px; }
  .bslib-sidebar-layout > .sidebar { border-right:1px solid %4$s !important; }
  .sec { color:%3$s; font-size:0.7rem; font-weight:500; text-transform:uppercase;
         letter-spacing:0.06em; margin:4px 0 6px; }
  .card { box-shadow:none; }
  .card-header { background:transparent; border-bottom:1px solid %4$s; font-size:0.8rem;
                 font-weight:500; color:%6$s; padding:10px 14px; }
  .card-header .sub { color:%3$s; font-weight:400; margin-left:6px; }
  .kpi { background:%7$s; border:1px solid %4$s; border-radius:8px; padding:14px 16px; height:100%%; }
  .kpi .lbl { color:%2$s; font-size:0.72rem; margin-bottom:6px; display:flex; align-items:center; gap:6px; }
  .kpi .dot { width:7px; height:7px; border-radius:50%%; display:inline-block; }
  .kpi .val { font-size:1.55rem; font-weight:600; letter-spacing:-0.02em; line-height:1.1; }
  .kpi .foot { color:%3$s; font-size:0.72rem; margin-top:6px; white-space:nowrap;
               overflow:hidden; text-overflow:ellipsis; }
  .form-label, .control-label { color:%2$s; font-size:0.75rem; font-weight:500; }
  .form-control, .selectize-input { font-size:0.82rem; }
  .selectize-input, .selectize-dropdown { background:#141516 !important; border-color:%4$s !important; }
  .selectize-input .item { background:rgba(94,106,210,0.18) !important; color:#C9CDFB !important;
                           border:0 !important; border-radius:4px !important; }
  .form-check-input { background-color:#141516; border-color:rgba(255,255,255,0.18); }
  .form-check-input:checked { background-color:%5$s; border-color:%5$s; }
  .form-check-label { font-size:0.8rem; }
  .stage-sw { display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:6px; }
  .irs--shiny .irs-bar { background:%5$s; border-color:%5$s; }
  .irs--shiny .irs-line { background:#1C1D1F; border-color:%4$s; }
  .irs--shiny .irs-handle { background:%6$s; border:0; box-shadow:0 0 0 3px rgba(94,106,210,0.35); }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:%5$s; font-size:0.7rem; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background:#1C1D1F; color:%2$s; font-size:0.68rem; }
  .btn-lin { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.78rem; }
  .btn-lin:hover { background:#1C1D1F; color:%6$s; }
  .meta { color:%3$s; font-size:0.72rem; line-height:1.5; }
  .meta a { color:%2$s; }
  .osm-dark { filter: invert(100%%) hue-rotate(180deg) grayscale(55%%) brightness(0.82) contrast(1.05); }
  .leaflet-container { background:%1$s; font-family:Inter, sans-serif; }
  .leaflet-popup-content-wrapper, .leaflet-popup-tip { background:#141516; color:%6$s;
       border:1px solid %4$s; box-shadow:0 8px 24px rgba(0,0,0,0.5); }
  .leaflet-popup-content { margin:12px 14px; font-size:0.78rem; line-height:1.55; }
  .leaflet-popup-content a { color:#A5ABF2; }
  .pop-k { color:%2$s; }
  .pop-stage { display:inline-block; font-size:0.68rem; padding:1px 7px; border-radius:999px;
               border:1px solid currentColor; margin-bottom:4px; }
  .info.legend, .size-legend { background:rgba(15,16,17,0.92) !important; color:%6$s !important;
       border:1px solid %4$s; border-radius:6px; font-size:0.72rem; box-shadow:none; padding:8px 10px; }
  .size-legend .t { color:%2$s; margin-bottom:4px; }
  .leaflet-bar a, .leaflet-control-layers { background:#141516; color:%6$s; border-color:%4$s; }
  .leaflet-control-layers-expanded { font-size:0.75rem; }
  .leaflet-control-attribution { background:rgba(8,9,10,0.75) !important; color:%3$s; }
  .leaflet-control-attribution a { color:%2$s; }
  table.dataTable { font-size:0.78rem; }
  table.dataTable thead th { color:%2$s; font-weight:500; border-bottom:1px solid %4$s !important; }
  table.dataTable tbody td { border-top:1px solid rgba(255,255,255,0.04) !important; }
  table.dataTable tbody tr:hover > * { background:rgba(255,255,255,0.03) !important; }
  table.dataTable tbody tr.selected > * { box-shadow:inset 0 0 0 9999px rgba(94,106,210,0.22) !important;
       color:%6$s !important; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_filter label { color:%3$s; font-size:0.74rem; }
  .dt-buttons .btn { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.74rem; padding:3px 10px; }
  .page-link { background:transparent; border-color:%4$s; color:%2$s; }
  .active > .page-link { background:%5$s; border-color:%5$s; }
  ", lin$bg, lin$muted, lin$faint, lin$border, lin$accent, lin$text, lin$surface))

kpi <- function(label, color, value_id, foot_id) {
  div(class = "kpi",
      div(class = "lbl", span(class = "dot", style = paste0("background:", color)), label),
      div(class = "val", textOutput(value_id, inline = TRUE)),
      div(class = "foot", textOutput(foot_id, inline = TRUE)))
}

plotly_lin <- function(p, ...) {
  base <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, sans-serif", color = lin$muted, size = 11),
    margin = list(l = 8, r = 8, t = 6, b = 6),
    hoverlabel = list(bgcolor = lin$raised, bordercolor = "#2A2B2E",
                      font = list(color = lin$text, family = "Inter")),
    xaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 linecolor = lin$border, tickfont = list(color = lin$faint)),
    yaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 tickfont = list(color = lin$faint)),
    showlegend = FALSE
  )
  do.call(layout, c(list(p), modifyList(base, list(...)))) |>
    config(displayModeBar = FALSE)
}

fmt_mw <- function(x) {
  ifelse(x >= 1000, sprintf("%.1f GW", x / 1000),
         paste(formatC(x, format = "f", digits = 1, big.mark = ","), "MW"))
}

stage_labels <- lapply(STAGES, \(s) HTML(sprintf(
  "<span class='stage-sw' style='background:%s'></span>%s", STAGE_COLS[[s]], s)))

size_legend_html <- function() {
  refs <- c(10, 100, 500)
  r <- bubble_r(refs); h <- 2 * max(r) + 4
  circles <- paste0(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' stroke='#8A8F98' stroke-width='1'/>
     <text x='%.1f' y='%.1f' fill='#8A8F98' font-size='10' text-anchor='middle'>%s</text>",
    max(r) + 2, h - r - 2, r, max(r) + 2, h - 2 * r - 5,
    c("10", "100", "500 MW")), collapse = "")
  sprintf("<div class='size-legend'><div class='t'>Bubble area = MW</div>
          <svg width='%d' height='%d'>%s</svg></div>", ceiling(2 * max(r) + 30), ceiling(h + 12), circles)
}

# ---- UI ---------------------------------------------------------------------

ui <- function(req) {
d <- current_data()
yr_rng <- d$yr_rng; SITE_MW_MAX <- d$site_max

page_sidebar(
  theme = theme, fillable = FALSE, window_title = "Mexico Solar PV Fleet",
  title = div(class = "brand", span(class = "logo"), "Mexico Solar PV Fleet",
              span(class = "tag", "GEM Solar Tracker",
                   if (!is.null(d$raw$permits)) " · CNE permits")),
  sidebar = sidebar(
    width = 290, bg = lin$bg,
    div(class = "sec", "Layers"),
    checkboxGroupInput("stages", NULL, choiceNames = stage_labels,
                       choiceValues = STAGES, selected = DEFAULT_STAGES),
    div(class = "sec", "Filters"),
    selectizeInput("state", "State", choices = d$states, multiple = TRUE,
                   options = list(placeholder = "All states")),
    if (length(d$schemes))
      selectizeInput("scheme", "Permit scheme (CNE)", choices = d$schemes, multiple = TRUE,
                     options = list(placeholder = "All schemes")),
    sliderInput("mw", "Site size (MW)", min = 0, max = SITE_MW_MAX,
                value = c(1, SITE_MW_MAX), step = 1, ticks = FALSE),
    sliderInput("yr", "Start year (actual or expected)", min = yr_rng[1], max = yr_rng[2],
                value = yr_rng, step = 1, sep = "", ticks = FALSE),
    checkboxInput("no_year", "Include sites without a start year", value = TRUE),
    textInput("q", "Search site, owner or permit holder", placeholder = "e.g. Enel"),
    actionButton("reset", "Reset filters", class = "btn-lin w-100"),
    hr(style = sprintf("border-color:%s; opacity:1; margin:14px 0 10px;", lin$border)),
    div(class = "sec", "Data"),
    uiOutput("meta")
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    kpi("Operating",          STAGE_COLS[["Operating"]],          "k_op",  "k_op_f"),
    kpi("Under construction", STAGE_COLS[["Under construction"]], "k_uc",  "k_uc_f"),
    kpi("Active pipeline",    STAGE_COLS[["Pre-construction"]],   "k_pl",  "k_pl_f"),
    kpi("Largest operating site", "#D4D8FF",                      "k_big", "k_big_f")
  ),

  card(
    full_screen = TRUE, height = "620px",
    card_header("Sites", span(class = "sub",
      "one bubble per site and stage · filled = operating, dashed ring = pipeline")),
    card_body(padding = 0, leafletOutput("map", height = "100%"))
  ),

  card(
    full_screen = TRUE, height = "430px",
    card_header("Capacity trajectory", span(class = "sub",
      "annual additions by start year · cumulative fleet if pipeline stages complete")),
    card_body(plotlyOutput("trajectory", height = "100%"))
  ),

  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(height = "400px", full_screen = TRUE,
         card_header("Capacity by state", span(class = "sub", "top 15, selected stages")),
         card_body(plotlyOutput("by_state", height = "100%"))),
    card(height = "400px", full_screen = TRUE,
         card_header("Site size distribution", span(class = "sub", "count of sites by stage")),
         card_body(plotlyOutput("by_size", height = "100%")))
  ),

  card(
    full_screen = TRUE,
    card_header("Capacity by owner", span(class = "sub",
      "GEM owner field · MW · respects all filters")),
    card_body(DTOutput("owners"))
  ),

  card(
    full_screen = TRUE,
    card_header("Sites", span(class = "sub", "select a row to fly to it on the map")),
    card_body(DTOutput("sites_tbl"))
  ),

  if (!is.null(d$raw$permits)) card(
    full_screen = TRUE,
    card_header("CNE solar generation permits", span(class = "sub",
      "every solar permit in the file · exact and strong matches are attached to sites, possible ones are shown here only")),
    card_body(DTOutput("permits_tbl"))
  )
)
}

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  D <- reactiveVal(current_data())

  output$meta <- renderUI({
    s <- D()$raw
    div(class = "meta",
        "Plants: Global Energy Monitor, ",
        a("Global Solar Power Tracker", href = GEM_URL, target = "_blank"),
        " (", s$gem_file, ", CC BY 4.0). Phases rolled up to one bubble per site and stage. ",
        "MW as reported by GEM (MWac where known). Pipeline phases with a start year already ",
        "past are shown in ", CY, ".",
        br(), br(),
        if (!is.null(s$permits)) {
          cf <- table(factor(s$permits$match_conf, c("exact", "strong", "possible", "none")))
          tagList(sprintf(
            "Permits: CNE %s, %s solar permits. Matched to GEM sites: %d exact (permit number), %d strong (coordinates or name), %d possible (municipality and capacity only, not attached), %d unmatched. %d sites carry a permit.",
            s$permit_file, format(nrow(s$permits), big.mark = ","), cf[["exact"]], cf[["strong"]],
            cf[["possible"]], cf[["none"]], n_distinct(s$gens$loc_id[!is.na(s$gens$permit_no)])))
        } else tagList(
          "Optional: download the ",
          a("CNE generation permits export", href = CNE_URL, target = "_blank"),
          " and drop it in the app folder (keep its ue_per_gen_hist name, or any name ",
          "containing 'permis') to add permit number, holder and scheme."),
        br(), br(),
        "Map tiles © ",
        a("OpenStreetMap", href = "https://www.openstreetmap.org/copyright", target = "_blank"),
        " contributors.",
        br(), sprintf("Built %s", format(s$built, "%Y-%m-%d %H:%M")))
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "stages", selected = DEFAULT_STAGES)
    updateSelectizeInput(session, "state", selected = character(0))
    updateSelectizeInput(session, "scheme", selected = character(0))
    updateSliderInput(session, "mw", value = c(1, D()$site_max))
    updateSliderInput(session, "yr", value = D()$yr_rng)
    updateCheckboxInput(session, "no_year", value = TRUE)
    updateTextInput(session, "q", value = "")
  })

  q_d <- debounce(reactive(input$q), 300)

  # Phase-level rows after attribute filters (all stages)
  gens <- reactive({
    d <- D()$G |> filter((is.na(year_plot) & isTRUE(input$no_year)) |
                         (!is.na(year_plot) & year_plot >= input$yr[1] & year_plot <= input$yr[2]))
    if (length(input$state))  d <- filter(d, state %in% input$state)
    if (length(input$scheme)) d <- filter(d, scheme %in% input$scheme)
    q <- trimws(q_d())
    if (nzchar(q)) d <- filter(d, grepl(q, plant, ignore.case = TRUE) |
                                  grepl(q, owner, ignore.case = TRUE) |
                                  grepl(q, coalesce(operator, ""), ignore.case = TRUE) |
                                  grepl(q, coalesce(permit_holder, ""), ignore.case = TRUE))
    d
  })

  # One row per site and stage: the bubble map unit
  sites_all <- reactive({
    gens() |>
      arrange(year_plot) |>
      group_by(loc_id, stage) |>
      summarise(
        plant = first(plant), owner = first(owner), operator = first(operator),
        state = first(state), municipality = first(municipality),
        status = paste(unique(status), collapse = "; "),
        lat = first(lat), lon = first(lon), mw = sum(mw), n_ph = n(),
        rating = paste(unique(na.omit(rating)), collapse = "/"),
        yr_first = suppressWarnings(min(year, na.rm = TRUE)),
        yr_last  = suppressWarnings(max(year, na.rm = TRUE)),
        accuracy = first(accuracy), permit_no = first(permit_no),
        permit_holder = first(permit_holder), scheme = first(scheme),
        permit_status = first(permit_status), permit_conf = first(permit_conf),
        n_permits = first(n_permits), permit_list = first(permit_list),
        wiki = first(wiki), .groups = "drop") |>
      mutate(yr_first = ifelse(is.finite(yr_first), yr_first, NA_integer_),
             yr_last  = ifelse(is.finite(yr_last),  yr_last,  NA_integer_)) |>
      filter(mw >= input$mw[1], mw <= input$mw[2])
  })

  sites <- reactive({
    sites_all() |>
      filter(stage %in% input$stages) |>
      mutate(size_class = cut(mw, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
             radius = bubble_r(mw),
             online = ifelse(is.na(yr_first), "-", as.character(yr_first))) |>
      arrange(desc(mw))
  })

  # KPIs (ignore the layer toggles so they always tell the full story)
  stage_sum <- function(st) {
    s <- sites_all() |> filter(stage %in% st)
    list(mw = sum(s$mw), n = nrow(s))
  }
  output$k_op   <- renderText(fmt_mw(stage_sum("Operating")$mw))
  output$k_op_f <- renderText(sprintf("%s sites · %.0f%% of MX fleet",
    format(stage_sum("Operating")$n, big.mark = ","),
    100 * stage_sum("Operating")$mw / max(D()$total_op, 1)))
  output$k_uc   <- renderText(fmt_mw(stage_sum("Under construction")$mw))
  output$k_uc_f <- renderText({
    s <- sites_all() |> filter(stage == "Under construction")
    if (!nrow(s)) return("no sites")
    yrs <- na.omit(c(s$yr_first, s$yr_last))
    if (length(yrs)) sprintf("%s sites · start %s to %s", nrow(s),
                             max(min(yrs), CY), max(yrs))
    else sprintf("%s sites", nrow(s))
  })
  output$k_pl   <- renderText(fmt_mw(stage_sum(PIPELINE)$mw))
  output$k_pl_f <- renderText({
    s <- stage_sum(PIPELINE); op <- stage_sum("Operating")$mw
    st <- stage_sum(STALLED)
    sprintf("%s sites · %s of today's fleet · %s stalled",
            format(s$n, big.mark = ","),
            if (op > 0) sprintf("%.0f%%", 100 * s$mw / op) else "-", fmt_mw(st$mw))
  })
  big <- reactive(sites_all() |> filter(stage == "Operating") |>
                    slice_max(mw, n = 1, with_ties = FALSE))
  output$k_big   <- renderText(if (nrow(big())) fmt_mw(big()$mw) else "-")
  output$k_big_f <- renderText(if (nrow(big())) sprintf("%s, %s", big()$plant, big()$state) else "")

  # ---- Map (OpenStreetMap tiles, no key) ----
  output$map <- renderLeaflet({
    osm_attr <- '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    osm_url  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM dark",
               options = tileOptions(maxZoom = 19, className = "osm-dark")) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM standard",
               options = tileOptions(maxZoom = 19)) |>
      addLayersControl(baseGroups = c("OSM dark", "OSM standard"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(-102.5, 23.8, zoom = 5) |>
      addLegend("bottomright", colors = unname(STAGE_COLS), labels = STAGES,
                title = "Stage", opacity = 1) |>
      addControl(html = HTML(size_legend_html()), position = "bottomleft",
                 className = "size-legend-wrap")
  })

  observe({
    d <- sites() |> mutate(stage = as.character(stage))
    proxy <- leafletProxy("map") |> clearGroup("sites") |> clearGroup("focus")
    if (!nrow(d)) return()
    col <- unname(STAGE_COLS[d$stage])
    wiki_link <- ifelse(is.na(d$wiki) | !nzchar(d$wiki), "",
                        sprintf("<br><a href='%s' target='_blank'>GEM wiki page</a>", d$wiki))
    permit <- ifelse(is.na(d$permit_no), "",
      sprintf("<br><span class='pop-k'>CNE permit</span> %s &middot; %s<br>
               <span class='pop-k'>Holder</span> %s<br>
               <span class='pop-k'>Scheme</span> %s%s",
              htmlEscape(d$permit_no), htmlEscape(na_dash(d$permit_status)),
              htmlEscape(na_dash(d$permit_holder)), htmlEscape(na_dash(d$scheme)),
              ifelse(d$n_permits > 1,
                     sprintf(" &middot; <span title='%s'>%d permits on file</span>",
                             htmlEscape(d$permit_list), d$n_permits), "")))
    pop <- sprintf(
      "<span class='pop-stage' style='color:%s'>%s</span><br>
       <b>%s</b><br>
       <span class='pop-k'>Owner</span> %s<br>
       <span class='pop-k'>Operator</span> %s<br>
       <span class='pop-k'>Location</span> %s, %s<br>
       <span class='pop-k'>Capacity</span> %s MW%s &middot; %d phase%s<br>
       <span class='pop-k'>Start year</span> %s<br>
       <span class='pop-k'>Status</span> %s<br>
       <span class='pop-k'>Coordinates</span> %s%s%s",
      col, d$stage, htmlEscape(d$plant), htmlEscape(d$owner), htmlEscape(na_dash(d$operator)),
      htmlEscape(na_dash(d$municipality)), htmlEscape(na_dash(d$state)),
      formatC(d$mw, format = "f", digits = 1, big.mark = ","),
      ifelse(nzchar(d$rating), paste0(" (", d$rating, ")"), ""),
      d$n_ph, ifelse(d$n_ph == 1, "", "s"), d$online, htmlEscape(d$status),
      htmlEscape(na_dash(d$accuracy)), permit, wiki_link)
    lab <- sprintf("%s · %s MW · %s", d$plant, formatC(d$mw, format = "f", digits = 1), d$stage)

    is_op <- d$stage == "Operating"
    if (any(is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[is_op], lat = d$lat[is_op], radius = d$radius[is_op],
        group = "sites", layerId = paste0(d$loc_id[is_op], "_op"),
        stroke = TRUE, weight = 0.8, color = col[is_op], opacity = 0.9,
        fillColor = col[is_op], fillOpacity = 0.45,
        popup = pop[is_op], label = lab[is_op])
    }
    if (any(!is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[!is_op], lat = d$lat[!is_op], radius = d$radius[!is_op],
        group = "sites", layerId = paste0(d$loc_id[!is_op], "_", d$stage[!is_op]),
        stroke = TRUE, weight = 1.4, dashArray = "3,3", color = col[!is_op], opacity = 0.95,
        fillColor = col[!is_op], fillOpacity = 0.10,
        popup = pop[!is_op], label = lab[!is_op])
    }
  })

  # ---- Capacity trajectory ----
  output$trajectory <- renderPlotly({
    stg <- setdiff(input$stages, STALLED)
    g <- gens() |> filter(!is.na(year_plot), stage %in% stg)
    shiny::validate(shiny::need(nrow(g) > 0,
                                "No capacity with a start year for the current filters."))

    yrs <- seq(min(g$year_plot), max(c(g$year_plot, CY)))
    annual <- g |>
      group_by(year_plot, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop") |>
      complete(year_plot = yrs, stage = factor(STAGES, STAGES), fill = list(gw = 0))

    p <- plot_ly()
    for (s in intersect(STAGES, stg)) {
      a <- filter(annual, stage == s, gw > 0)
      if (!nrow(a)) next
      p <- p |> add_bars(data = a, x = ~year_plot, y = ~gw, name = s,
                         marker = list(color = STAGE_COLS[[s]],
                                       opacity = if (s == "Operating") 1 else 0.85),
                         hovertemplate = paste0(s, " · %{x}: +%{y:.2f} GW<extra></extra>"))
    }

    op <- annual |> filter(stage == "Operating") |> arrange(year_plot) |>
      mutate(cum = cumsum(gw))
    hist_end <- max(c(g$year_plot[g$stage == "Operating"], CY - 1), na.rm = TRUE)
    base <- sum(op$gw)
    if ("Operating" %in% stg) {
      h <- filter(op, year_plot <= hist_end)
      p <- p |> add_lines(data = h, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = "Operating fleet (cumulative)",
                          line = list(color = "#D4D8FF", width = 2),
                          hovertemplate = "%{x}: %{y:.2f} GW installed<extra></extra>")
    } else base <- 0

    fut_years <- yrs[yrs >= hist_end]
    pipe_on <- intersect(PIPELINE, stg)
    scen <- list(c("Under construction"),
                 c("Under construction", "Pre-construction"),
                 PIPELINE)
    scen_name <- c("+ under construction", "+ pre-construction", "+ announced")
    dash <- c("dash", "dashdot", "dot")
    for (i in seq_along(scen)) {
      st <- intersect(scen[[i]], pipe_on)
      if (!length(st) || (i > 1 && setequal(st, intersect(scen[[i - 1]], pipe_on)))) next
      add <- annual |> filter(stage %in% st, year_plot >= hist_end) |>
        group_by(year_plot) |> summarise(gw = sum(gw), .groups = "drop") |>
        right_join(tibble(year_plot = fut_years), by = "year_plot") |>
        mutate(gw = coalesce(gw, 0)) |> arrange(year_plot) |>
        mutate(cum = base + cumsum(gw))
      add <- bind_rows(tibble(year_plot = hist_end, cum = base), add)
      p <- p |> add_lines(data = add, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = scen_name[i],
                          line = list(color = STAGE_COLS[[st[length(st)]]], width = 1.6,
                                      dash = dash[i]),
                          hovertemplate = paste0(scen_name[i], " · %{x}: %{y:.2f} GW<extra></extra>"))
    }

    p |> plotly_lin(
      barmode = "stack", bargap = 0.22, showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10, color = lin$muted),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(title = "", dtick = 2, tickfont = list(color = lin$faint),
                   range = c(max(min(yrs), CY - 16) - 0.5, max(yrs) + 0.5)),
      yaxis = list(title = list(text = "GW added per year", font = list(size = 10)),
                   gridcolor = "rgba(255,255,255,0.05)", tickfont = list(color = lin$faint)),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = "cumulative GW", font = list(size = 10)),
                    tickfont = list(color = lin$faint)),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = CY - 0.5, x1 = CY - 0.5, y0 = 0, y1 = 1,
                         line = list(color = "rgba(255,255,255,0.25)", width = 1, dash = "dot"))),
      annotations = list(list(x = CY - 0.5, y = 1, xref = "x", yref = "paper",
                              text = paste(CY, "→ pipeline"), showarrow = FALSE,
                              xanchor = "left", yanchor = "top", xshift = 4,
                              font = list(size = 10, color = lin$faint))),
      margin = list(l = 8, r = 44, t = 34, b = 6))
  })

  output$by_state <- renderPlotly({
    s <- sites_all() |> filter(stage %in% input$stages) |>
      group_by(state, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop")
    shiny::validate(shiny::need(nrow(s) > 0, "No sites for the current filters."))
    top <- s |> group_by(state) |> summarise(t = sum(gw)) |> slice_max(t, n = 15) |> arrange(t)
    s <- filter(s, state %in% top$state) |> mutate(state = factor(state, top$state))
    p <- plot_ly()
    for (st in intersect(STAGES, unique(as.character(s$stage)))) {
      p <- p |> add_bars(data = filter(s, stage == st), x = ~gw, y = ~state,
                         orientation = "h", name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{y}: %{x:.2f} GW<extra></extra>"))
    }
    p |> plotly_lin(barmode = "stack", bargap = 0.3,
                    xaxis = list(title = list(text = "GW", font = list(size = 10))),
                    yaxis = list(title = "", tickfont = list(color = lin$muted)))
  })

  output$by_size <- renderPlotly({
    z <- sites() |> count(size_class, stage, .drop = FALSE) |>
      filter(stage %in% input$stages)
    shiny::validate(shiny::need(sum(z$n) > 0, "No sites for the current filters."))
    p <- plot_ly()
    for (st in intersect(STAGES, input$stages)) {
      p <- p |> add_bars(data = filter(z, stage == st), x = ~size_class, y = ~n, name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{x}: %{y:,} sites<extra></extra>"))
    }
    p |> plotly_lin(barmode = "group", bargap = 0.25, bargroupgap = 0.08,
                    xaxis = list(title = "", tickfont = list(color = lin$muted)),
                    yaxis = list(title = ""))
  })

  # ---- Capacity by owner ----
  owner_tbl <- reactive({
    sites_all() |>
      group_by(Owner = owner) |>
      summarise(
        `Operating sites`     = sum(stage == "Operating"),
        `Operating MW`        = sum(mw[stage == "Operating"]),
        `Under constr. MW`    = sum(mw[stage == "Under construction"]),
        `Pre-construction MW` = sum(mw[stage == "Pre-construction"]),
        `Announced MW`        = sum(mw[stage == "Announced"]),
        `Stalled MW`          = sum(mw[stage %in% STALLED]),
        States = n_distinct(state),
        `Largest site` = {
          o <- stage == "Operating"
          if (any(o)) sprintf("%s (%s MW)", plant[o][which.max(mw[o])],
                              formatC(max(mw[o]), format = "f", digits = 0))
          else "-"
        },
        .groups = "drop") |>
      mutate(`Total active` = `Operating MW` + `Under constr. MW` +
               `Pre-construction MW` + `Announced MW`,
             `Share of operating` = `Operating MW` / max(sum(`Operating MW`), 1)) |>
      relocate(`Share of operating`, .after = `Operating MW`) |>
      arrange(desc(`Operating MW`), desc(`Total active`))
  })

  output$owners <- renderDT({
    d <- owner_tbl()
    datatable(
      d, rownames = FALSE, selection = "none", extensions = "Buttons",
      style = "bootstrap5", class = "compact hover",
      options = list(pageLength = 15, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(2, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(1:8, 10))))
    ) |>
      formatRound(c("Operating MW", "Under constr. MW", "Pre-construction MW",
                    "Announced MW", "Stalled MW", "Total active"), digits = 0) |>
      formatPercentage("Share of operating", digits = 1) |>
      formatStyle("Operating MW",
                  background = styleColorBar(c(0, max(d$`Operating MW`, 1)),
                                             "rgba(94,106,210,0.35)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Under constr. MW",
                  background = styleColorBar(c(0, max(d$`Under constr. MW`, 1)),
                                             "rgba(242,153,74,0.30)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---- Site table ----
  output$sites_tbl <- renderDT({
    datatable(
      sites() |> transmute(Site = plant, Stage = stage, Owner = owner, State = state,
                           Municipality = municipality, MW = round(mw, 1), Rating = rating,
                           Phases = n_ph, Start = online, Coordinates = accuracy,
                           Permit = permit_no, `Permit status` = permit_status,
                           Permits = ifelse(n_permits > 0, n_permits, NA), Scheme = scheme),
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      extensions = "Buttons",
      options = list(pageLength = 12, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(5, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(5, 7))))
    ) |>
      formatStyle("Stage", color = styleEqual(STAGES, unname(STAGE_COLS)))
  })

  observeEvent(input$sites_tbl_rows_selected, {
    r <- sites()[input$sites_tbl_rows_selected, ]
    leafletProxy("map") |>
      flyTo(r$lon, r$lat, zoom = 13, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$lon, lat = r$lat, radius = r$radius + 6, group = "focus",
                       stroke = TRUE, color = "#FFFFFF", weight = 1.5, opacity = 0.9,
                       fill = FALSE)
  })

  # ---- Permits table ----
  CONF_COLS <- c(exact = "#5E6AD2", strong = "#A5ABF2", possible = "#F2C94C", none = "#62666D")

  # Permits with a location to fly to: their own coordinates, else the matched site
  permit_rows <- reactive({
    P <- D()$raw$permits
    req(!is.null(P))
    site_xy <- D()$G |> group_by(loc_id) |>
      summarise(s_lat = first(lat), s_lon = first(lon), .groups = "drop")
    P |> left_join(site_xy, by = "loc_id") |>
      mutate(fly_lat = coalesce(lat, s_lat), fly_lon = coalesce(lon, s_lon),
             conf_order = match(match_conf, names(CONF_COLS))) |>
      arrange(conf_order, desc(mw))
  })

  output$permits_tbl <- renderDT({
    P <- permit_rows()
    datatable(
      P |> transmute(Permit = permit_no, Holder = holder, Status = status, Scheme = scheme,
                     MW = round(mw, 1), State = state, Municipality = municipality,
                     Granted = granted, `Has coords` = ifelse(is.na(lat), "", "yes"),
                     Match = match_conf, `Matched site` = matched_site, How = match_how),
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      filter = "top", extensions = "Buttons",
      options = list(pageLength = 12, dom = "Bftip", buttons = list("csv", "excel"),
                     ordering = TRUE, order = list(),
                     columnDefs = list(list(className = "dt-right", targets = 4)))) |>
      formatStyle("Match", color = styleEqual(names(CONF_COLS), unname(CONF_COLS)),
                  fontWeight = "500")
  })

  observeEvent(input$permits_tbl_rows_selected, {
    r <- permit_rows()[input$permits_tbl_rows_selected, ]
    if (is.na(r$fly_lat)) {
      showNotification("This permit has no coordinates and no matched site.", duration = 4)
      return()
    }
    leafletProxy("map") |>
      flyTo(r$fly_lon, r$fly_lat, zoom = 12, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$fly_lon, lat = r$fly_lat, radius = 14, group = "focus",
                       stroke = TRUE, color = "#F2C94C", weight = 1.5, opacity = 0.95,
                       fill = FALSE, label = sprintf("%s · %s", r$permit_no, r$holder))
  })
}

shinyApp(ui, server)
