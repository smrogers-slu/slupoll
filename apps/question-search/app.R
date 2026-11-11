# SLU/YouGov Poll: Question Search
#
# A searchable table of every question asked in the poll, June 2020 to
# August 2026. Modeled on the CES "Question Search" app (CES Files folder),
# styled with the SLU design system (08 Style), and built to work on phones.
#
# Data: data/question_catalog.csv, built from the Master Codebook by
# data-raw/build_question_catalog.R. Rebuild that file when the codebook changes.
#
# Run locally from the repo root:  shiny::runApp("apps/question-search")

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(htmltools)

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

# Wave choices for the filter, newest first. The value is the wave code
# (the project's key for a wave); the label is the readable name.
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
  @media (min-width: 576px) { table.dataTable td.dtr-control { min-width: 9.5rem; } }
  .wave-cell { display: inline-block; vertical-align: top; white-space: nowrap; }
  .wave-cell .var { display: block; color: #555; font-size: .9rem;
                    font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
  table.dataTable ul.dtr-details li { padding: 6px 0; }
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
  title = "SLU/YouGov Poll Question Search",

  div(class = "app-header",
    div(class = "container-lg",
      img(src = "slu-yougov-poll-logo.png", alt = "SLU/YouGov Poll"),
      h1("Question Search")
    )
  ),

  div(class = "container-lg",
    p(class = "intro",
      sprintf("Search the wording of every question asked in the SLU/YouGov Poll, %s to %s.",
              first_wave, last_wave)),
    p(class = "intro",
      "Type one or more words. A question is shown if it contains all of them, in its wording,",
      "its short description or its variable name. Tap the arrow beside a wave to see",
      "answer options and the margin of error."),

    card(
      card_body(
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(8, 4)),
          textInput("search_terms", "Search questions",
                    placeholder = "Try: abortion, taxes, POLS029",
                    width = "100%"),
          selectizeInput("waves", "Waves", choices = wave_choices, multiple = TRUE,
                         width = "100%",
                         options = list(placeholder = "All waves",
                                        plugins = list("remove_button")))
        ),
        div(class = "result-count", textOutput("result_count", inline = TRUE)),
        DTOutput("results")
      )
    ),

    tags$footer(class = "app-footer",
      "Question wording comes from the SLU/YouGov Poll Master Codebook (August 2026 update).",
      "Variable names are assigned fresh in each wave, so the same name in two waves",
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
        # Wave and variable stacked in one narrow cell so it fits on a phone.
        `Wave` = sprintf('<span class="wave-cell">%s<span class="var">%s</span></span>',
                         htmlEscape(wave_name), htmlEscape(variable)),
        `Question` = htmlEscape(wording),
        `Short description` = htmlEscape(description),
        `Answer options` = htmlEscape(coalesce(answer_options, "")),
        `Margin of error` = ifelse(is.na(moe), "Not listed",
                                   sprintf("plus or minus %s points", moe))
      )

    datatable(
      d,
      rownames = FALSE,
      escape = FALSE,          # cells were escaped above; this lets the Wave cell keep its line break
      selection = "none",
      extensions = "Responsive",
      class = "stripe hover",
      options = list(
        dom = "tip",           # table, info, paging (our own search box replaces DT's)
        pageLength = 25,
        ordering = FALSE,      # keep newest-first order
        responsive = list(details = list(type = "column", target = 0)),
        columnDefs = list(
          list(className = "dtr-control", orderable = FALSE, targets = 0),
          list(responsivePriority = 1, targets = c(0, 1)),
          list(className = "desktop", targets = 2),
          list(className = "none", targets = c(3, 4))
        ),
        language = list(
          info = "_START_ to _END_ of _TOTAL_",
          infoEmpty = "",
          zeroRecords = "No questions match. Try fewer or different words, or clear the wave filter.",
          paginate = list(previous = "Previous", `next` = "Next")
        )
      )
    )
  })
}

shinyApp(ui, server)
