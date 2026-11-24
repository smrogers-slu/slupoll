# Build every issue's page, plus one combined page with an Issue menu.
#
# The combined page (apps/issue-graphs/index.html) holds all issues in one
# file, so it can be published on Posit Connect Cloud as a single Static
# Document. Each issue still has its own link (for example
# ...#issue=issue-ratings) and its own page in apps/issue-graphs/<issue>/.
#
# Run from the repo root:  Rscript data-raw/issue-graphs/build_all_issue_graphs.R
# To add an issue: write its "series - <Issue>.csv" and add its name below.

issues <- c("Abortion", "Issue Ratings")

entries <- list()
for (issue_name in issues) {
  source(file.path("data-raw", "issue-graphs", "build_issue_graphs.R"))
  entries[[issue_name]] <- issue_entry
}
write_issue_page(entries, "Issue Trend Graphs", file.path("apps", "issue-graphs", "index.html"))

# Posit Connect Cloud needs a manifest.json next to the page it publishes.
# It lists the page files with a checksum, so it must be rewritten every time
# the pages change. One for the combined page, and one for each issue's page.
rsconnect::writeManifest(file.path("apps", "issue-graphs"), appPrimaryDoc = "index.html")
for (e in entries) {
  rsconnect::writeManifest(file.path("apps", "issue-graphs", e$slug), appPrimaryDoc = "index.html")
}
message("Updated manifest.json files for Posit Connect Cloud.")
