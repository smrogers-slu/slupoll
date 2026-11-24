# Build the trend graphs for one Issue Summary topic.
#
# What this does, in plain language:
#   1. Reads "series - <Issue>.csv" in data-raw/issue-graphs. Each row says which
#      question (wave code + question ID) belongs to which graph. Rows with the
#      same series_id are the same question asked in different polls, and the
#      why_grouped column says which library source links them.
#   2. Opens each poll's public data file and works out, for all voters and
#      for each demographic group (party, gender, age, race, income, education,
#      community type, region, ideology), the weighted percent giving each answer.
#   3. Checks those numbers against the published Issue Summary results.
#   4. Writes an HTML page: the questions are listed on the left, with a
#      demographic filter under them (one filter at a time), and the chosen
#      question's graph on the right.
#
# Where things go:
#   The page:   apps/issue-graphs/<issue>/index.html in this repo. It holds only
#               rounded results for groups of 31 or more, so it is safe to publish.
#               Posit Connect Cloud publishes it as a "Static Document".
#   The full results table and the check against published numbers go to
#   Aditi Working/Issue Graphs/<Issue> in Dropbox (not into this public repo).
#
# Run it from the repo root. Set `issue_name` first to pick the issue:
#   issue_name <- "Issue Ratings"
#   source("data-raw/issue-graphs/build_issue_graphs.R")
# or from the Terminal:  Rscript data-raw/issue-graphs/build_issue_graphs.R "Issue Ratings"
# With no issue given it builds Abortion. A new issue needs its own
# "series - <Issue>.csv" file in data-raw/issue-graphs.
#
# Series file columns:
#   variable  the question ID. When a poll asked two halves of the sample the
#             same question under two IDs, list both joined by "+" (for
#             example POLS012a+POLS012b); they are combined into one result.
#   collapse  optional. How to combine answers, for example
#             "Excellent or good=Excellent+Good|Fair or poor=Fair+Poor|Not sure=Not sure".
#             Blank means every answer is shown on its own.
#
# House rules followed (RA Reference Library, 03 Guides/05 Analysis Conventions.md):
#   - Percentages weighted with `weight`; "Not sure" stays in the base.
#   - Only `skipped` and `not asked` are dropped, found by their value label.
#   - A point is hidden unless at least 31 unweighted respondents were asked.
#   - Categories are combined from unrounded shares, then rounded once.
#   - Groups follow 03 Guides/04 Demographic Groups.md. Party, gender, race and
#     education come from each poll's file; age, income, community type, region
#     and ideology from 01 Data/Derived/all-waves-standard-groups.csv, which
#     fixes the codes that differ between polls. People who chose "Other" or
#     "Not sure" for party, or "prefer not to say" for income, are in All voters
#     but in no group of that filter.
#   - Question wording comes from the Master Codebook.

args <- commandArgs(trailingOnly = TRUE)
issue <- if (exists("issue_name")) issue_name else if (length(args)) args[1] else "Abortion"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(jsonlite)
})

here <- file.path("data-raw", "issue-graphs")   # series files and the page template
if (!dir.exists(here)) stop("Run this script from the repo root (could not find ", here, ").")
work_dir <- Sys.getenv("SLUPOLL_WORKING",
                       unset = path.expand("~/Dropbox/Dashboard/Aditi Working"))
library_dir <- Sys.getenv("SLUPOLL_LIBRARY",
                          unset = path.expand("~/Dropbox/Dashboard/RA Reference Library"))
# Full results and checks (not published).
out_dir <- file.path(work_dir, "Issue Graphs", issue)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# The published page, e.g. apps/issue-graphs/issue-ratings/index.html
page_dir <- file.path("apps", "issue-graphs", gsub("[^a-z0-9]+", "-", tolower(issue)))
dir.create(page_dir, showWarnings = FALSE, recursive = TRUE)

min_n <- 31

# ---- Poll dates --------------------------------------------------------------
# Field dates from 03 Guides/01 Poll Overview and Waves.md (first and last
# interview in each data file). Each dot is placed at the middle of the
# field period.
polls <- tibble::tribble(
  ~wave_code, ~wave_name,      ~field_start,  ~field_end,    ~field_label,
  "STLU0001", "June 2020",     "2020-06-23",  "2020-07-01",  "June 23 to July 1, 2020",
  "STLU0002", "October 2020",  "2020-09-24",  "2020-10-07",  "Sept. 24 to Oct. 7, 2020",
  "STLU0003", "July 2021",     "2021-07-19",  "2021-07-29",  "July 19 to 29, 2021",
  "STLU0006", "August 2022",   "2022-08-08",  "2022-08-16",  "Aug. 8 to 16, 2022",
  "STLU0007", "February 2023", "2023-02-08",  "2023-02-14",  "Feb. 8 to 14, 2023",
  "STLU0009", "August 2023",   "2023-07-27",  "2023-08-08",  "July 27 to Aug. 8, 2023",
  "STLU0011", "February 2024", "2024-02-14",  "2024-02-26",  "Feb. 14 to 26, 2024",
  "STLU0012", "August 2024",   "2024-08-08",  "2024-08-16",  "Aug. 8 to 16, 2024",
  "STLU0013", "February 2025", "2025-02-18",  "2025-03-02",  "Feb. 18 to March 2, 2025",
  "STLU0015", "February 2026", "2026-02-09",  "2026-02-22",  "Feb. 9 to 22, 2026",
  "STLU0016", "August 2026",   "2026-08-13",  "2026-08-24",  "Aug. 13 to 24, 2026"
) |>
  mutate(across(c(field_start, field_end), as.Date),
         field_mid = field_start + as.numeric(field_end - field_start) / 2)

# ---- Inputs ------------------------------------------------------------------
series <- read.csv(file.path(here, paste0("series - ", issue, ".csv")),
                   stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
stopifnot(all(series$wave_code %in% polls$wave_code),
          !any(duplicated(series[c("wave_code", "variable")])))

codebook <- read.csv(file.path(library_dir, "02 Codebook",
                               "SLU_Poll_Master_Codebook - August 2026 Update.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8") |>
  mutate(wave_name = sub("\\s+Poll$", "", Poll))

data_dirs <- list.dirs(file.path(library_dir, "01 Data", "Poll Data"), recursive = FALSE)
poll_data <- list()   # each wave's data, read once
read_wave <- function(wave_name) {
  if (is.null(poll_data[[wave_name]])) {
    folder <- data_dirs[endsWith(data_dirs, paste0(" ", wave_name))]
    sav <- list.files(folder, pattern = "\\.sav$", full.names = TRUE)
    stopifnot(length(sav) == 1)
    poll_data[[wave_name]] <<- read_sav(sav)
  }
  poll_data[[wave_name]]
}

# ---- Demographic groups -------------------------------------------------------
# One filter at a time; the groups are those in 03 Guides/04 Demographic Groups.md
# (standard groups, plus region and ideology from the optional list).
breakdowns <- list(
  "Party"          = c("Democrats", "Republicans", "Independents"),
  "Gender"         = c("Men", "Women"),
  "Age"            = c("18-29", "30-44", "45-64", "65+"),
  "Race"           = c("White", "Black", "All non-white"),
  "Income"         = c("Under $50k", "$50k - $100k", "$100k or more"),
  "Education"      = c("HS or less", "Some college", "4-year college", "Post-grad"),
  "Community type" = c("Rural", "Urban", "Suburban"),
  "Region"         = c("Kansas City Metro Area", "St. Louis Metro Area", "Northeast Missouri",
                       "Northwest Missouri", "Southeast Missouri", "Southwest Missouri"),
  "Ideology"       = c("Liberal", "Moderate", "Conservative")
)
std_groups <- read.csv(file.path(library_dir, "01 Data", "Derived", "all-waves-standard-groups.csv"),
                       stringsAsFactors = FALSE, na.strings = "", colClasses = c(caseid = "character"))

# Stop if a variable's codes do not mean what we expect in this poll.
check_codes <- function(d, v, expected) {
  labs <- attr(d[[v]], "labels")
  if (!identical(unname(labs[names(expected)]), unname(expected)))
    stop(v, " codes are not as expected in this poll")
}

# For one poll: a TRUE/FALSE column per group, one row per respondent.
# Column names look like "Party|Democrats"; "All voters" is everyone.
group_members <- function(d, wave_code) {
  check_codes(d, "pid3", c(Democrat = 1, Republican = 2, Independent = 3))
  check_codes(d, "gender", c(Male = 1, Female = 2))
  check_codes(d, "race", c(White = 1, Black = 2, skipped = 98, "not asked" = 99))
  check_codes(d, "educ", c("No HS" = 1, "High school graduate" = 2, "Some college" = 3,
                           "2-year" = 4, "4-year" = 5, "Post-grad" = 6))
  sg <- std_groups[std_groups$wave_code == wave_code, ]
  sg <- sg[match(as.character(d$caseid), sg$caseid), ]
  if (anyNA(sg$caseid)) stop("Some respondents are missing from the standard-groups file")
  num  <- function(v) as.numeric(unclass(d[[v]]))
  race <- num("race")
  out <- list(
    "All voters"              = rep(TRUE, nrow(d)),
    "Party|Democrats"         = num("pid3") %in% 1,
    "Party|Republicans"       = num("pid3") %in% 2,
    "Party|Independents"      = num("pid3") %in% 3,
    "Gender|Men"              = num("gender") %in% 1,
    "Gender|Women"            = num("gender") %in% 2,
    "Race|White"              = race %in% 1,
    "Race|Black"              = race %in% 2,
    "Race|All non-white"      = !is.na(race) & !(race %in% c(1, 98, 99)),
    "Education|HS or less"    = num("educ") %in% 1:2,
    "Education|Some college"  = num("educ") %in% 3:4,
    "Education|4-year college" = num("educ") %in% 5,
    "Education|Post-grad"     = num("educ") %in% 6
  )
  for (g in breakdowns$Age)              out[[paste0("Age|", g)]]            <- sg$agegroup_label %in% g
  for (g in breakdowns$Income)           out[[paste0("Income|", g)]]         <- sg$incgroup_label %in% g
  for (g in breakdowns$`Community type`) out[[paste0("Community type|", g)]] <- sg$hrsa_label %in% g
  for (g in breakdowns$Region)           out[[paste0("Region|", g)]]         <- sg$mo_region %in% g
  for (g in breakdowns$Ideology)         out[[paste0("Ideology|", g)]]       <- sg$ideology3 %in% g
  expected <- c("All voters", unlist(lapply(names(breakdowns), function(b) paste0(b, "|", breakdowns[[b]]))))
  stopifnot(setequal(names(out), expected))
  as.data.frame(out[expected], check.names = FALSE)
}
is_dropped <- function(lab) tolower(trimws(lab)) %in% c("skipped", "not asked")
# "Not Sure" and "Not sure" are the same answer; compare answers ignoring case.
key <- function(x) tolower(trimws(x))
tidy_label <- function(x) ifelse(key(x) == "not sure", "Not sure", trimws(x))

# Turn "Agree=Strongly Agree+Agree|Disagree=..." into a lookup:
# answer label (lower case) -> category shown on the graph.
parse_collapse <- function(spec) {
  if (is.na(spec) || spec == "") return(NULL)
  parts <- strsplit(strsplit(spec, "|", fixed = TRUE)[[1]], "=", fixed = TRUE)
  out <- character(0)
  for (p in parts) {
    for (src in strsplit(p[2], "+", fixed = TRUE)[[1]]) out[key(src)] <- trimws(p[1])
  }
  out
}

# ---- Compute -----------------------------------------------------------------
results <- list()   # what the graphs show (after any combining of answers)
raw     <- list()   # every answer separately, for checking against published files

for (i in seq_len(nrow(series))) {
  s <- series[i, ]
  wave_name <- polls$wave_name[polls$wave_code == s$wave_code]
  d <- read_wave(wave_name)
  members <- group_members(d, s$wave_code)

  # Each respondent's answer as text, or NA if they were not asked. Split-sample
  # versions ("POLS012a+POLS012b") are combined; nobody may have answered both.
  answer_all <- rep(NA_character_, nrow(d))
  opts_text  <- character(0)
  for (v in strsplit(s$variable, "+", fixed = TRUE)[[1]]) {
    if (!v %in% names(d)) stop(v, " is not in the ", wave_name, " data")
    labs <- attr(d[[v]], "labels")
    val  <- as.numeric(unclass(d[[v]]))
    asked <- !is.na(val) & !(val %in% labs[is_dropped(names(labs))])
    opts <- labs[!is_dropped(names(labs))]
    if (!all(val[asked] %in% opts)) stop(v, " (", wave_name, ") has answers without labels")
    if (any(asked & !is.na(answer_all))) stop("Someone answered more than one of ", s$variable)
    answer_all[asked] <- names(opts)[match(val[asked], opts)]
    opts_text <- c(opts_text, names(opts)[!key(names(opts)) %in% key(opts_text)])
  }
  keep   <- !is.na(answer_all)
  answer <- answer_all[keep]
  opts   <- setNames(seq_along(opts_text), opts_text)
  w      <- d$weight[keep]
  members <- members[keep, , drop = FALSE]

  lookup <- parse_collapse(s$collapse)
  category <- if (is.null(lookup)) tidy_label(answer) else unname(lookup[key(answer)])
  if (anyNA(category)) stop("collapse rule for ", s$series_id, " misses an answer")
  cat_order <- unique(if (is.null(lookup)) tidy_label(names(opts)) else unname(lookup))

  for (g in names(members)) {
    in_g <- members[[g]]
    n <- sum(in_g)
    share <- if (n == 0) rep(NA_real_, length(cat_order)) else
      vapply(cat_order, function(cc) sum(w[in_g & category == cc]) / sum(w[in_g]), numeric(1))
    results[[length(results) + 1]] <- data.frame(
      series_id = s$series_id, wave_code = s$wave_code, variable = s$variable,
      sample_n = nrow(d), group = g, category = cat_order, category_order = seq_along(cat_order),
      share = unname(share), n = n, shown = n >= min_n)
    # Every answer separately, for all voters and party, to check against published results.
    if (g == "All voters" || startsWith(g, "Party|")) {
      raw_share <- vapply(names(opts), function(o) sum(w[in_g & answer == o]) / sum(w[in_g]), numeric(1))
      raw[[length(raw) + 1]] <- data.frame(
        wave_name = wave_name, variable = s$variable, group = sub("^Party\\|", "", g),
        answer = names(opts), share = unname(raw_share), n = n)
    }
  }
}
results <- bind_rows(results) |> left_join(polls, by = "wave_code")
raw     <- bind_rows(raw)

# Every series must use the same answer categories in every poll.
cats <- results |> distinct(series_id, wave_code, category) |>
  group_by(series_id, wave_code) |> summarise(k = paste(sort(key(category)), collapse = "|"), .groups = "drop") |>
  group_by(series_id) |> summarise(same = n_distinct(k) == 1)
if (!all(cats$same)) stop("Answer categories differ across polls in: ",
                          paste(cats$series_id[!cats$same], collapse = ", "))

# ---- Check against the published Issue Summary results --------------------------
pub_file <- file.path(library_dir, "04 Published Results", "Issue Summary Results",
                      paste0("IssueSummary-", gsub(" ", "", issue), "-Distributions.csv"))
check <- NULL
if (file.exists(pub_file)) {
  pub <- read.csv(pub_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                  colClasses = "character") |>
    mutate(wave_name = sub(" SLU/YouGov Poll$", "", Poll))
  pub_long <- bind_rows(lapply(c("All voters" = "AllVoters", Democrats = "Democrats",
                                 Republicans = "Republicans", Independents = "Independents"),
    function(col) transmute(pub, wave_name, variable = VariableName, answer_key = key(Response),
                            published = as.numeric(sub("%", "", .data[[col]])),
                            published_n = as.numeric(UnweightedN))), .id = "group")
  check <- raw |> mutate(answer_key = key(answer), ours = round(100 * share)) |>
    inner_join(pub_long, by = c("wave_name", "variable", "group", "answer_key")) |>
    mutate(difference = ours - published,
           n_note = ifelse(group == "All voters" & n != published_n,
                           sprintf("we have %d asked, published %d", n, published_n), "")) |>
    select(wave_name, variable, group, answer, ours, published, difference, n_note)
  write.csv(check, file.path(out_dir, "check against published.csv"), row.names = FALSE)
  in_series <- nrow(distinct(series, wave_code, variable))
  message(sprintf("Checked %d percentages against the published %s results: %d match, %d differ.",
                  nrow(check), issue, sum(check$difference == 0), sum(check$difference != 0)))
  if (any(check$difference != 0)) print(check[check$difference != 0, ], row.names = FALSE)
}

# ---- Save the numbers --------------------------------------------------------
results |>
  transmute(series_id, wave_code, wave_name, field_label, variable,
            filter = ifelse(group == "All voters", "", sub("\\|.*$", "", group)),
            group = sub("^.*\\|", "", group), category,
            percent = round(100 * share), share = round(share, 6), n_asked = n, shown) |>
  write.csv(file.path(out_dir, paste(issue, "results.csv")), row.names = FALSE)

# ---- Build the page ----------------------------------------------------------
wording <- codebook |> select(wave_name, variable = VariableName, wording = `Question Wording`, moe = MoE)

# "Agree=Strongly Agree+Agree|..." becomes "Strongly agree and agree are combined, ..."
collapse_note <- function(spec) {
  if (is.na(spec) || spec == "") return(NULL)
  parts <- strsplit(strsplit(spec, "|", fixed = TRUE)[[1]], "=", fixed = TRUE)
  joined <- vapply(parts, function(p) {
    src <- tolower(trimws(strsplit(p[2], "+", fixed = TRUE)[[1]]))
    if (length(src) < 2) "" else paste(src, collapse = " and ")
  }, character(1))
  joined <- joined[joined != ""]
  if (!length(joined)) return(NULL)
  out <- paste0(joined[1], " are combined",
                if (length(joined) > 1) paste0(", as are ", paste(joined[-1], collapse = ", and ")) else "", ".")
  paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
}

graphs <- lapply(unique(series$series_id), function(id) {
  rows <- series[series$series_id == id, ] |>
    left_join(polls, by = "wave_code") |> arrange(field_mid) |>
    mutate(first_id = sub("\\+.*$", "", variable), pooled = grepl("+", variable, fixed = TRUE)) |>
    left_join(wording, by = c("wave_name", "first_id" = "variable")) |>
    mutate(moe_num = suppressWarnings(as.numeric(moe)))
  # Each half of a combined split sample has its own margin of error in the codebook.
  arm_moe <- function(v, wave) {
    ids <- strsplit(v, "+", fixed = TRUE)[[1]]
    m <- wording$moe[wording$wave_name == wave & wording$variable %in% ids]
    paste(sprintf("plus or minus %s", m), collapse = " and ")
  }
  r <- results[results$series_id == id, ]
  all_n <- r[r$group == "All voters" & r$category_order == 1, ]
  rows$n_all    <- all_n$n[match(rows$wave_code, all_n$wave_code)]
  rows$sample_n <- all_n$sample_n[match(rows$wave_code, all_n$wave_code)]
  cats <- unique(r$category[order(r$category_order)])
  # Results for every group: one entry per poll (in poll order) of
  # [number asked, percent for each answer]; a percent is null when hidden.
  by_group <- lapply(split(r, r$group), function(g) {
    lapply(rows$wave_code, function(wc) {
      x <- g[g$wave_code == wc, ][order(g$category_order[g$wave_code == wc]), ]
      c(list(x$n[1]), lapply(seq_len(nrow(x)), function(k) if (x$shown[k]) round(100 * x$share[k]) else NULL))
    })
  })
  partial <- rows[!rows$pooled & rows$n_all < 0.75 * rows$sample_n, ]
  list(
    id = id,
    title = rows$title[1],
    collapse_note = collapse_note(rows$collapse[1]),
    pooled_note = if (any(rows$pooled)) paste0(
      paste(rows$wave_name[rows$pooled], collapse = " and "),
      " asked this question of two halves of the sample under two question IDs; the halves are combined here.") else NULL,
    partial_note = if (nrow(partial)) paste0(
      "Asked of only part of the sample in ",
      paste(sprintf("%s (%s of %s people)", partial$wave_name, format(partial$n_all, big.mark = ","),
                    format(partial$sample_n, big.mark = ",")), collapse = ", "), ".") else NULL,
    # Wording is shown once, as asked in the latest poll.
    wording = rows$wording[nrow(rows)],
    polls = lapply(seq_len(nrow(rows)), function(j) list(
      wave = rows$wave_name[j], variable = gsub("+", " and ", rows$variable[j], fixed = TRUE),
      dates = rows$field_label[j], mid = format(rows$field_mid[j]), wording = rows$wording[j],
      moe = if (rows$pooled[j]) NULL else rows$moe_num[j],
      moe_text = if (rows$pooled[j]) paste0("not listed for the combined halves (",
                   arm_moe(rows$variable[j], rows$wave_name[j]), " points for each half)") else NULL,
      partial = !rows$pooled[j] && rows$n_all[j] < 0.75 * rows$sample_n[j])),
    categories = I(cats),
    data = by_group
  )
})

# One issue's part of the page. A page can hold one issue or several
# (build_all_issue_graphs.R puts them all on one page with an Issue menu).
issue_entry <- list(
  issue = issue,
  slug  = gsub("[^a-z0-9]+", "-", tolower(issue)),
  graphs = unname(graphs)
)

write_issue_page <- function(entries, title, out_file) {
  page_data <- list(
    built = format(Sys.Date(), "%B %d, %Y"),
    min_n = min_n,
    breakdowns = breakdowns,
    axis_start = "2019-07-01", axis_end = "2027-01-01",
    all_polls = lapply(seq_len(nrow(polls)), function(j)
      list(wave = polls$wave_name[j], mid = format(polls$field_mid[j]))),
    issues = unname(entries)
  )
  template <- paste(readLines(file.path(here, "issue_graphs_template.html"), encoding = "UTF-8"),
                    collapse = "\n")
  json <- toJSON(page_data, auto_unbox = TRUE, null = "null", digits = NA)
  json <- gsub("</", "<\\/", json, fixed = TRUE)   # keep the data from closing the script tag
  page <- gsub("/*TITLE*/", title, template, fixed = TRUE)
  page <- sub("/*DATA*/", json, page, fixed = TRUE)
  writeLines(page, out_file, useBytes = TRUE)
  message("Wrote ", sum(sapply(entries, function(e) length(e$graphs))), " graphs to ", out_file)
}

write_issue_page(list(issue_entry), paste(issue, "Trend Graphs"), file.path(page_dir, "index.html"))
