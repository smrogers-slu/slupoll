# Build the data for the dashboard's Issue Trends tab, for every issue.
#
# Runs build_issue_graphs.R once per issue, then saves four small tables in
# apps/question-search/data, which the dashboard app reads:
#   issue_trend_questions.csv  one row per graph: title, wording, notes
#   issue_trend_polls.csv      one row per graph per poll: question ID, margin of error
#   issue_trend_results.csv    weighted percents by poll and group (blank when
#                              fewer than 31 people in the group were asked)
#   poll_dates.csv             field dates of every poll, for the timeline
#
# Run from the repo root:  Rscript data-raw/issue-graphs/build_all_issue_graphs.R
# Then regenerate the app's manifest and push (see CLAUDE.md).
# To add an issue: write its "series - <Issue>.csv" and add its name below.

issues <- c("Abortion", "Issue Ratings")

questions <- list(); poll_rows <- list(); result_rows <- list()
for (issue_name in issues) {
  source(file.path("data-raw", "issue-graphs", "build_issue_graphs.R"))
  questions[[issue_name]]   <- issue_questions
  poll_rows[[issue_name]]   <- issue_polls
  result_rows[[issue_name]] <- issue_results
}

app_data <- file.path("apps", "question-search", "data")
save_table <- function(x, name) {
  write.csv(x, file.path(app_data, name), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
save_table(bind_rows(questions), "issue_trend_questions.csv")
save_table(bind_rows(poll_rows), "issue_trend_polls.csv")
save_table(bind_rows(result_rows), "issue_trend_results.csv")
save_table(transmute(polls, wave_code, wave_name, field_label, field_mid = format(field_mid)), "poll_dates.csv")
# The filter menu's groups, in order.
save_table(bind_rows(lapply(names(breakdowns), function(b)
  data.frame(filter = b, group = breakdowns[[b]], group_order = seq_along(breakdowns[[b]])))),
  "issue_trend_groups.csv")
message("Saved the Issue Trends tables in ", app_data)
