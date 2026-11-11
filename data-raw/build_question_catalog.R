# Build the question catalog used by the Question Search app.
#
# What this does, in plain language:
#   It reads the Master Codebook (one row per question per wave) from the
#   RA Reference Library, keeps only the columns the search app needs, cleans
#   them lightly, and saves a small CSV inside the app folder. The app reads
#   that CSV, so the app never needs the library itself (important for hosting).
#
# When to run it:
#   Whenever the Master Codebook is updated (for example after a new wave).
#   From the repo root:  Rscript data-raw/build_question_catalog.R
#
# Where it looks for the library:
#   The SLUPOLL_LIBRARY environment variable if it is set, otherwise the usual
#   Dropbox location on a Mac. The library is read-only; nothing is written there.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

library_dir <- Sys.getenv(
  "SLUPOLL_LIBRARY",
  unset = "~/Dropbox/Dashboard/RA Reference Library"
)
codebook_path <- file.path(
  library_dir, "02 Codebook", "SLU_Poll_Master_Codebook - August 2026 Update.csv"
)
out_path <- file.path("apps", "question-search", "data", "question_catalog.csv")

if (!file.exists(codebook_path)) {
  stop("Master Codebook not found at: ", codebook_path,
       "\nSet SLUPOLL_LIBRARY to the RA Reference Library folder.")
}
if (!dir.exists(dirname(out_path))) {
  stop("Run this script from the repo root (could not find ", dirname(out_path), ").")
}

codebook <- read_csv(codebook_path, show_col_types = FALSE, guess_max = 1e5)

# Tidy a block of text: turn line breaks into spaces and squeeze extra spaces.
clean_text <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Answer options: the codebook stores them across string_code_1 ... string_code_33.
# We join them into one line for display. "skipped" and "not asked" are
# housekeeping codes, not answers (Analysis Conventions, section 1), so they are left out.
answers <- codebook |>
  mutate(row_id = row_number()) |>
  select(row_id, starts_with("string_code_")) |>
  pivot_longer(-row_id, names_to = "slot", values_to = "label",
               values_drop_na = TRUE) |>
  mutate(slot = as.integer(sub("string_code_", "", slot)),
         label = clean_text(label)) |>
  filter(!tolower(label) %in% c("skipped", "not asked")) |>
  arrange(row_id, slot) |>
  group_by(row_id) |>
  summarise(answer_options = paste(label, collapse = " / "), .groups = "drop")

catalog <- codebook |>
  mutate(row_id = row_number()) |>
  left_join(answers, by = "row_id") |>
  transmute(
    # Wave code from the YouGov codebook filename, e.g. "stlu0006_codebook.pdf" -> "STLU0006".
    wave_code   = toupper(sub("_codebook\\.pdf$", "", Filename, ignore.case = TRUE)),
    # Display name, e.g. "August 2026 Poll" -> "August 2026".
    wave_name   = sub("\\s+Poll$", "", Poll),
    # A real date (first of the month) so waves sort in time order, not alphabetically.
    wave_date   = as.Date(paste("1", wave_name), format = "%d %B %Y"),
    variable    = VariableName,
    wording     = clean_text(`Question Wording`),
    description = clean_text(Description),
    moe         = MoE,
    answer_options,
    codebook_row = row_id   # keeps the codebook's own question order within a wave
  ) |>
  arrange(desc(wave_date), codebook_row)

# Checks: stop loudly if anything looks off, rather than writing a bad file.
stopifnot(
  !anyNA(catalog$wave_date),
  all(grepl("^STLU[0-9]{4}$", catalog$wave_code)),
  n_distinct(catalog$wave_code) == 11,
  !any(duplicated(catalog[c("wave_code", "variable")]))
)

write_csv(catalog, out_path, na = "")
message("Wrote ", nrow(catalog), " questions from ",
        n_distinct(catalog$wave_code), " waves to ", out_path)
