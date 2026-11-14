# Build the results that the Question Search app charts in each drop-down.
#
# What this does:
#   For every question in the question catalog, it opens that wave's public
#   data file and works out the overall (all respondents) result:
#     - questions with answer options: the weighted share choosing each option
#     - numeric questions (0 to 100 sliders and the like): the weighted mean
#       and weighted median
#   It writes one small summary table. No respondent-level data leaves the
#   library; only these totals go into the app.
#
# When to run it:
#   After build_question_catalog.R, whenever the codebook or a data file changes.
#   From the repo root:  source("data-raw/build_question_results.R")
#
# House rules followed (03 Guides/05 Analysis Conventions.md):
#   - Percentages are weighted with `weight`.
#   - "Not sure" stays in. Only `skipped` and `not asked` are dropped, and they
#     are found by their value label, not by a code number.
#   - A result is kept only if at least 31 unweighted respondents were asked.
#   - Shares are stored unrounded; the app rounds once when it displays them.
#   - The weighted median is computed the same way as the poll's demographic
#     tables (05 Scripts/Demographic Tables).

library(haven)
library(dplyr)

library_dir <- Sys.getenv(
  "SLUPOLL_LIBRARY",
  unset = path.expand("~/Dropbox/Dashboard/RA Reference Library")
)
data_dir <- file.path(library_dir, "01 Data", "Poll Data")
if (!dir.exists(data_dir)) {
  stop("Cannot find the poll data at:\n  ", data_dir,
       "\nSet SLUPOLL_LIBRARY to the RA Reference Library folder.")
}

catalog_path <- "apps/question-search/data/question_catalog.csv"
out_path     <- "apps/question-search/data/question_results.csv"
notes_path   <- "apps/question-search/data/question_results_notes.csv"

min_n <- 31   # unweighted respondents needed before a result is shown

# Questions left out on purpose (03 Guides/06 Data Oddities and Known Issues.md).
# `why` is shown in the app; `detail` is for us.
left_out <- tibble::tribble(
  ~wave_code, ~variable, ~why,                           ~detail,
  "STLU0016", "EDU002",  "this question is under review", "placeholder answer labels in the data (oddity 14)",
  "STLU0013", "EDU001",  "this question is under review", "result looks wrong, may be a different question (oddity 13)"
)

catalog <- read.csv(catalog_path, stringsAsFactors = FALSE, na.strings = "",
                    encoding = "UTF-8")
waves <- distinct(catalog, wave_code, wave_name)

# The poll's weighted median (same as the demographic tables script).
w_median <- function(x, w) {
  o <- order(x); x <- x[o]; w <- cumsum(w[o]) / sum(w[o])
  x[which(w >= 0.5)[1]]
}

is_dropped_label <- function(lab) tolower(trimws(lab)) %in% c("skipped", "not asked")

results <- list()   # one data frame per question that gets a result
notes   <- list()   # one row per question that does not, with the reason
note <- function(wave_code, variable, why, detail = why) {
  notes[[length(notes) + 1]] <<- data.frame(wave_code, variable, why, detail)
}

wave_dirs <- list.dirs(data_dir, recursive = FALSE)

for (i in seq_len(nrow(waves))) {
  code <- waves$wave_code[i]
  # Wave folders are named like "2026-08 August 2026".
  folder <- wave_dirs[endsWith(wave_dirs, paste0(" ", waves$wave_name[i]))]
  sav <- list.files(folder, pattern = "\\.sav$", full.names = TRUE)
  stopifnot(length(folder) == 1, length(sav) == 1)
  d <- read_sav(sav)
  stopifnot("weight" %in% names(d), !anyNA(d$weight))

  for (v in catalog$variable[catalog$wave_code == code]) {
    skip <- left_out[left_out$wave_code == code & left_out$variable == v, ]
    if (nrow(skip) == 1)   { note(code, v, skip$why, skip$detail); next }
    if (!v %in% names(d))  { note(code, v, "this question is not in the public data file"); next }

    x <- d[[v]]
    if (is.character(x))   { note(code, v, "answers were typed in as text"); next }

    labs <- attr(x, "labels")           # named vector: names are the answer text
    val  <- as.numeric(unclass(x))
    w    <- d$weight

    # Drop `skipped` and `not asked` (by label) and anyone with no answer recorded.
    drop_codes <- labs[is_dropped_label(names(labs))]
    asked <- !is.na(val) & !(val %in% drop_codes)
    val <- val[asked]; w <- w[asked]
    n <- length(val)
    if (n < min_n) {
      note(code, v, "too few respondents were asked",
           sprintf("only %d respondents asked (need %d)", n, min_n))
      next
    }

    options <- labs[!is_dropped_label(names(labs))]

    if (length(options) == 0) {
      # No answer options: a number the respondent typed or slid to.
      results[[length(results) + 1]] <- data.frame(
        wave_code = code, variable = v, kind = "numeric", n = n,
        option_order = NA_integer_, option = NA_character_, share = NA_real_,
        mean = sum(w * val) / sum(w), median = w_median(val, w))
    } else if (all(val %in% options)) {
      # Answer options: weighted share for each, in the questionnaire's order.
      share <- vapply(unname(options), function(o) sum(w[val == o]) / sum(w), numeric(1))
      results[[length(results) + 1]] <- data.frame(
        wave_code = code, variable = v, kind = "options", n = n,
        option_order = seq_along(options), option = trimws(names(options)),
        share = share, mean = NA_real_, median = NA_real_)
    } else {
      # For example August 2024 POLS037 and POLS038: a typed number plus a
      # "don't know" option. The guides do not say how to report these.
      note(code, v, "this question is under review",
           "numbers mixed with labelled answers; the guides do not say how to report it")
    }
  }
}

results <- bind_rows(results)
notes   <- bind_rows(notes)

# Checks: shares for each question add to 1, and nothing is duplicated.
sums <- results |> filter(kind == "options") |>
  group_by(wave_code, variable) |> summarise(s = sum(share), .groups = "drop")
stopifnot(
  all(abs(sums$s - 1) < 1e-9),
  !any(duplicated(results[c("wave_code", "variable", "option_order")])),
  all(results$n >= min_n)
)

results$share  <- round(results$share, 6)   # tidy storage only; display rounding happens in the app
results$mean   <- round(results$mean, 4)
results$median <- round(results$median, 4)
write.csv(results, out_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

n_q <- nrow(distinct(results, wave_code, variable))
message("Wrote results for ", n_q, " of ", nrow(catalog), " questions to ", out_path)
message("No result for ", nrow(notes), " questions:")
print(as.data.frame(count(notes, detail, sort = TRUE)), row.names = FALSE)
write.csv(notes, notes_path, row.names = FALSE, fileEncoding = "UTF-8")
