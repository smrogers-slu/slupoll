# Poll Question Search
#
# A searchable table of every question asked in the poll, June 2020 to
# August 2026. Modeled on the CES "Question Search" app (CES Files folder),
# styled with the SLU design system (08 Style), and built to work on phones.
#
# Branding: the page does not name the poll for now (Aditi, November 14, 2026).
# It shows the placeholder in `poll_name` below. To put the real name back,
# change that one line. The logo is kept in the repo's branding/ folder.
#
# Data (all in data/, each built by a script in data-raw/):
#   question_catalog.csv        the questions, from the Master Codebook
#                               (build_question_catalog.R)
#   question_results.csv        each question's overall weighted result
#   question_results_notes.csv  why some questions have no result
#                               (both from build_question_results.R)
#   published_graphs.csv        questions that have a published graph
#                               (build_published_graphs.R)
#
# Run locally from the repo root:  shiny::runApp("apps/question-search")

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(htmltools)

# ---- Name shown on the page -------------------------------------------------

# Placeholder until the branding is decided. Every place the page names the
# poll uses this, so this is the only line to change.
poll_name <- "Poll"

# ---- Data ------------------------------------------------------------------

catalog <- read.csv("data/question_catalog.csv",
                    stringsAsFactors = FALSE, na.strings = "",
                    encoding = "UTF-8") |>
  mutate(
    wave_date = as.Date(wave_date),
    # One lowercase string per question that the search box looks through:
    # question wording, short description and variable name.
    search_text = tolower(paste(wording, description, variable))
  ) |>
  arrange(desc(wave_date), codebook_row)

# ---- Results chart for each question ----------------------------------------

# One row per answer option (or one row for a numeric question). Shares are
# stored unrounded and rounded once here, as the poll's conventions require.
results <- read.csv("data/question_results.csv", stringsAsFactors = FALSE,
                    na.strings = "", encoding = "UTF-8")
result_notes <- read.csv("data/question_results_notes.csv", stringsAsFactors = FALSE,
                         encoding = "UTF-8")
published <- read.csv("data/published_graphs.csv", stringsAsFactors = FALSE,
                      encoding = "UTF-8")

# Big numbers (dollar amounts) get commas; small ones (0 to 100 sliders) one decimal.
show_number <- function(x) {
  ifelse(abs(x) >= 1000, format(round(x), big.mark = ",", trim = TRUE),
         format(round(x, 1), nsmall = 1, trim = TRUE))
}
asked_note <- function(n) {
  sprintf('<div class="res-note">Weighted results for everyone who was asked this question (%s respondents).</div>',
          format(n, big.mark = ","))
}

# A bar chart drawn with plain HTML: one line per answer option.
option_charts <- results |>
  filter(kind == "options") |>
  arrange(wave_code, variable, option_order) |>
  mutate(pct = round(100 * share),
         line = sprintf(
           '<div class="res-row"><span class="res-lab">%s</span><span class="res-track"><span class="res-bar" style="width:%d%%"></span></span><span class="res-pct">%d%%</span></div>',
           htmlEscape(option), pct, pct)) |>
  group_by(wave_code, variable) |>
  summarise(chart = paste0('<div class="res" role="img" aria-label="Bar chart of answers">',
                           paste(line, collapse = ""), '</div>', asked_note(first(n))),
            .groups = "drop")

number_charts <- results |>
  filter(kind == "numeric") |>
  transmute(wave_code, variable,
            chart = paste0(sprintf('<div class="res-num">Average: <strong>%s</strong> <span>Median: <strong>%s</strong></span></div>',
                                   show_number(mean), show_number(median)),
                           asked_note(n)))

charts <- bind_rows(option_charts, number_charts)

catalog <- catalog |>
  left_join(charts, by = c("wave_code", "variable")) |>
  left_join(result_notes |> select(wave_code, variable, why), by = c("wave_code", "variable")) |>
  left_join(published, by = c("wave_code", "variable")) |>
  mutate(
    chart = coalesce(chart, sprintf('<span class="res-none">No chart, because %s.</span>',
                                    coalesce(why, "no result is available"))),
    # The library does not record each graph's own web address, so this links
    # to the poll's results page for that wave, where the published graphs are.
    graph_link = ifelse(
      is.na(results_url),
      '<span class="res-none">No published graph is listed for this question.</span>',
      sprintf('&ldquo;%s&rdquo; is on the <a href="%s" target="_blank" rel="noopener">%s results page</a> (opens the poll website).',
              htmlEscape(graph_title), htmlEscape(results_url, attribute = TRUE),
              htmlEscape(wave_name)))
  )

# Choices for the "Poll" filter, newest first. Each poll is one wave. The value
# is the wave code (the project's key for a wave); the label is the readable name.
waves <- catalog |> distinct(wave_code, wave_name, wave_date) |> arrange(desc(wave_date))
wave_choices <- setNames(waves$wave_code, waves$wave_name)

first_wave <- waves$wave_name[nrow(waves)]
last_wave  <- waves$wave_name[1]

# ---- Look ------------------------------------------------------------------

# SLU design system colors (tokens/colors.css). Brandon Grotesque is not used
# on the dashboard (Steve, November 8, 2026); Oswald is the design system's
# own stand-in for headings, and Archivo Narrow is its face for tables.
slu_blue  <- "#003DA5"
grand_blue <- "#00244D"

theme <- bs_theme(
  version = 5,
  preset = "flatly",
  primary = slu_blue,
  secondary = grand_blue,
  bg = "#FFFFFF",
  fg = "#1A1A1A",
  base_font = font_google("Archivo Narrow", local = FALSE),
  heading_font = font_google("Oswald", local = FALSE),
  "link-color" = slu_blue,
  "border-radius" = "6px",
  # Flatly colors its page buttons teal; use SLU blues instead.
  "pagination-bg" = slu_blue,
  "pagination-color" = "#FFFFFF",
  "pagination-border-color" = slu_blue,
  "pagination-hover-bg" = grand_blue,
  "pagination-hover-border-color" = grand_blue,
  "pagination-active-bg" = grand_blue,
  "pagination-active-border-color" = grand_blue,
  "pagination-disabled-bg" = "#C8C9C7",
  "pagination-disabled-color" = "#FFFFFF",
  "pagination-disabled-border-color" = "#C8C9C7"
)

app_css <- "
  body { background: #f6f7f8; }
  .app-header { background: #fff; border-bottom: 4px solid #003DA5; padding: 16px 0 12px; margin-bottom: 20px; }
  .app-header img { max-width: 100%; height: auto; max-height: 34px; display: block; }
  .app-header h1 { font-size: 1.35rem; text-transform: uppercase; letter-spacing: .06em;
                   color: #00244D; margin: 12px 0 0; }
  .intro { color: #555; font-size: 1.05rem; margin-bottom: 4px; }
  .card { border: 1px solid #e6e6e6; box-shadow: 0 1px 3px rgba(0,0,0,.08); border-radius: 10px; }
  .form-control:focus, .selectize-input.focus { border-color: #53C3EE; box-shadow: 0 0 0 .2rem rgba(83,195,238,.35); }
  .result-count { color: #555; margin: 4px 0 10px; font-size: .98rem; }
  table.dataTable td { vertical-align: top; font-size: 1rem; line-height: 1.35; }
  table.dataTable thead th { color: #00244D; }
  table.dataTable td.dtr-control { white-space: nowrap; }
  .app-header .poll-name { font-family: Oswald, sans-serif; font-size: 1.5rem; font-weight: 600;
                           color: #003DA5; line-height: 1.1; }
  table.dataTable td.qid { white-space: nowrap; color: #555; font-size: .9rem;
                           font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
  table.dataTable ul.dtr-details { width: 100%; }
  table.dataTable ul.dtr-details li { padding: 6px 0; }
  .res { max-width: 640px; margin-top: 4px; }
  .res-row { display: grid; grid-template-columns: minmax(7rem, 14rem) 1fr 3rem; gap: 8px;
             align-items: center; padding: 2px 0; }
  .res-lab { font-size: .95rem; line-height: 1.2; white-space: normal; }
  .res-track { background: #e9ecef; border-radius: 3px; height: 14px; display: block; }
  .res-bar { background: #003DA5; border-radius: 3px; height: 14px; display: block; min-width: 1px; }
  .res-pct { text-align: right; font-variant-numeric: tabular-nums; font-weight: 600; }
  .res-num span { margin-left: 1.25rem; }
  .res-note, .res-none { color: #666; font-size: .9rem; }
  .res-note { margin-top: 4px; white-space: normal; }
  @media (max-width: 575.98px) {
    .res-row { grid-template-columns: 1fr 3rem; row-gap: 0; }
    .res-lab { grid-column: 1 / -1; }
  }
  table.dataTable .dtr-title { display: block; font-weight: 600; color: #00244D; }
  table.dataTable td.dtr-control:before { color: #003DA5; }
  footer.app-footer { color: #767676; font-size: .9rem; padding: 18px 0 28px; }
  @media (max-width: 575.98px) {
    .app-header h1 { font-size: 1.1rem; }
    table.dataTable td { font-size: .95rem; }
  }
"

# ---- UI --------------------------------------------------------------------

ui <- page_fluid(
  theme = theme,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(app_css))
  ),
  title = paste(poll_name, "Question Search"),

  div(class = "app-header",
    div(class = "container-lg",
      div(class = "poll-name", poll_name),
      h1("Question Search")
    )
  ),

  div(class = "container-lg",
    p(class = "intro",
      sprintf("Search the wording of every question asked in the poll, %s to %s.",
              first_wave, last_wave)),
    p(class = "intro",
      "Type one or more words. A question is shown if it contains all of them, in its wording,",
      "its short description or its question ID. Tap the arrow beside a poll to see",
      "a chart of the results, the answer options, the margin of error and",
      "where to find the published graph."),

    card(
      card_body(
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(8, 4)),
          textInput("search_terms", "Search questions",
                    placeholder = "Try: abortion, taxes, POLS029",
                    width = "100%"),
          selectizeInput("waves", "Poll", choices = wave_choices, multiple = TRUE,
                         width = "100%",
                         options = list(placeholder = "All polls",
                                        plugins = list("remove_button")))
        ),
        div(class = "result-count", textOutput("result_count", inline = TRUE)),
        DTOutput("results")
      )
    ),

    tags$footer(class = "app-footer",
      "Question wording comes from the poll's Master Codebook (August 2026 update).",
      "Question IDs are assigned fresh in each poll, so the same ID in two polls",
      "is not necessarily the same question."
    )
  )
)

# ---- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Wait until typing pauses briefly before searching (smoother on phones).
  search_terms <- debounce(reactive(input$search_terms), 300)

  filtered <- reactive({
    out <- catalog

    if (length(input$waves) > 0) {
      out <- out |> filter(wave_code %in% input$waves)
    }

    # Split the search box into words; every word must appear somewhere.
    words <- strsplit(tolower(trimws(search_terms() %||% "")), "\\s+")[[1]]
    words <- words[words != ""]
    for (w in words) {
      out <- out[grepl(w, out$search_text, fixed = TRUE), ]
    }
    out
  })

  output$result_count <- renderText({
    n <- nrow(filtered())
    total <- nrow(catalog)
    if (n == total) {
      sprintf("Showing all %s questions.", format(total, big.mark = ","))
    } else {
      sprintf("%s of %s questions match.", format(n, big.mark = ","),
              format(total, big.mark = ","))
    }
  })

  output$results <- renderDT({
    d <- filtered() |>
      transmute(
        # "Poll" is the wave name; "Question ID" is the variable name in that wave.
        `Poll` = htmlEscape(wave_name),
        `Question ID` = htmlEscape(variable),
        `Question` = htmlEscape(wording),
        `Short description` = htmlEscape(description),
        `Results` = chart,
        `Answer options` = htmlEscape(coalesce(answer_options, "")),
        `Margin of error` = ifelse(is.na(moe), "Not listed",
                                   sprintf("plus or minus %s points", moe)),
        `Published graph` = graph_link
      )

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,          # cells were already escaped above
      selection = "none",
      extensions = "Responsive",
      class = "stripe hover",
      options = list(
        dom = "tip",           # table, info, paging (our own search box replaces DT's)
        pageLength = 25,
        ordering = FALSE,      # keep newest-first order
        responsive = list(details = list(type = "column", target = 0)),
        columnDefs = list(
          # Column numbers start at 0: Poll, Question ID, Question,
          # Short description, Results, Answer options, Margin of error,
          # Published graph. The last four only show in the drop-down.
          list(className = "dtr-control", orderable = FALSE, targets = 0),
          list(className = "qid", targets = 1),
          # On a narrow screen keep Poll and Question first, then Question ID.
          list(responsivePriority = 1, targets = c(0, 2)),
          list(responsivePriority = 2, targets = 1),
          list(className = "desktop", targets = 3),
          list(className = "none", targets = c(4, 5, 6, 7))
        ),
        language = list(
          info = "_START_ to _END_ of _TOTAL_",
          infoEmpty = "",
          zeroRecords = "No questions match. Try fewer or different words, or clear the poll filter.",
          paginate = list(previous = "Previous", `next` = "Next")
        )
      )
    )
  })
}

shinyApp(ui, server)
