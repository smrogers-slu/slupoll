# Build the list of questions that have a published graph.
#
# What this does:
#   The poll's graph pipeline keeps one "Graph Information" file per wave
#   listing every question a graph was drawn for. This script reads those
#   files (06 Crosswalk Sources/Graph Information) and saves a small table:
#   wave, question ID, the graph's title, and the address of that wave's
#   results page on the poll website, where the published graphs are shown.
#
# What it cannot do:
#   The library does not record the web address of each individual graph, so
#   the app links to the wave's results page, not to the exact graph.
#
# When to run it: when a Graph Information file is added or changed.
#   From the repo root:  source("data-raw/build_published_graphs.R")

library(dplyr)

library_dir <- Sys.getenv(
  "SLUPOLL_LIBRARY",
  unset = path.expand("~/Dropbox/Dashboard/RA Reference Library")
)
gi_dir <- file.path(library_dir, "06 Crosswalk Sources", "Graph Information")
if (!dir.exists(gi_dir)) {
  stop("Cannot find the Graph Information folder at:\n  ", gi_dir,
       "\nSet SLUPOLL_LIBRARY to the RA Reference Library folder.")
}

catalog <- read.csv("apps/question-search/data/question_catalog.csv",
                    stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
waves <- distinct(catalog, wave_code, wave_name)
out_path <- "apps/question-search/data/published_graphs.csv"

# Results pages, as linked from the poll website's home page (checked
# November 14, 2026). They all follow the same pattern.
results_url <- function(wave_name) {
  paste0("https://www.slu.edu/research/research-institute/big-ideas/slu-poll/",
         "previous-poll-results/", gsub(" ", "-", tolower(wave_name)), "-results.php")
}

files <- list.files(gi_dir, pattern = "\\.csv$", full.names = TRUE)
graphs <- bind_rows(lapply(files, function(f) {
  # File names look like "SLU Poll - August 2026 - Generic Graph Information - Draft 11.csv".
  wave_name <- strsplit(basename(f), " - ", fixed = TRUE)[[1]][2]
  # These files are sideways: each row is a setting, each column is a question.
  g <- read.csv(f, header = FALSE, stringsAsFactors = FALSE,
                fileEncoding = "UTF-8-BOM", colClasses = "character")
  row_of <- function(name) trimws(unlist(g[g[[1]] == name, -1], use.names = FALSE))
  data.frame(
    wave_name   = wave_name,
    variable    = row_of("VariableName"),
    graph_title = trimws(paste(row_of("MainTitle_RowOne"), row_of("MainTitle_RowTwo")))
  )
})) |>
  filter(variable != "") |>
  inner_join(waves, by = "wave_name") |>
  mutate(results_url = results_url(wave_name)) |>
  select(wave_code, variable, graph_title, results_url)

in_catalog <- paste(graphs$wave_code, graphs$variable) %in%
  paste(catalog$wave_code, catalog$variable)
message(sum(in_catalog), " of ", nrow(graphs), " graphed questions match the question catalog.")
if (any(!in_catalog)) print(graphs[!in_catalog, c("wave_code", "variable", "graph_title")], row.names = FALSE)

stopifnot(
  length(files) == 11,
  n_distinct(graphs$wave_code) == 11,
  !any(duplicated(graphs[c("wave_code", "variable")]))
)
write.csv(graphs[in_catalog, ], out_path, row.names = FALSE, fileEncoding = "UTF-8")
message("Wrote ", sum(in_catalog), " rows to ", out_path)
