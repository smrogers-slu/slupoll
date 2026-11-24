# Issue Trends tab: one graph per question over time, by issue, with a
# one-at-a-time demographic filter.
#
# Shiny loads every file in this R/ folder automatically before app.R runs.
# app.R puts issue_trends_ui("trends") in a tab and calls
# issue_trends_server("trends").
#
# Data (in data/, built by data-raw/issue-graphs/build_all_issue_graphs.R):
#   issue_trend_questions.csv  one row per graph: issue, title, wording, notes
#   issue_trend_polls.csv      one row per graph per poll: question ID, margin of error
#   issue_trend_results.csv    rounded weighted percents by poll and group; blank
#                              when fewer than 31 people in the group were asked
#   issue_trend_groups.csv     the filter menu: each filter's groups, in order
#   poll_dates.csv             every poll's field dates; dots sit at the middle

# Shiny reads this file before app.R, so it loads its own packages.
library(shiny)
library(bslib)
library(dplyr)
library(plotly)

trend_questions <- read.csv("data/issue_trend_questions.csv", stringsAsFactors = FALSE,
                            na.strings = "", encoding = "UTF-8")
trend_polls     <- read.csv("data/issue_trend_polls.csv", stringsAsFactors = FALSE,
                            na.strings = "", encoding = "UTF-8")
trend_results   <- read.csv("data/issue_trend_results.csv", stringsAsFactors = FALSE,
                            na.strings = "", encoding = "UTF-8")
trend_groups    <- read.csv("data/issue_trend_groups.csv", stringsAsFactors = FALSE,
                            encoding = "UTF-8")
poll_dates      <- read.csv("data/poll_dates.csv", stringsAsFactors = FALSE,
                            encoding = "UTF-8") |>
  mutate(field_mid = as.Date(field_mid))

trend_results$filter[is.na(trend_results$filter)] <- ""
trend_issues  <- unique(trend_questions$issue)
trend_filters <- unique(trend_groups$filter)
min_n_shown   <- 31

# Design-system colors, in answer order; "Not sure" is always gray.
# Never red and never orange (05 Analysis Conventions, section 8).
answer_colors <- c("#00244D", "#53C3EE", "#795D3E", "#003DA5")
not_sure_gray <- "#8C8E8C"
color_for <- function(cats) {
  out <- rep(not_sure_gray, length(cats))
  others <- tolower(cats) != "not sure"
  out[others] <- answer_colors[(seq_len(sum(others)) - 1) %% length(answer_colors) + 1]
  setNames(out, cats)
}
# With more than this many polls, only the first and latest dots get a label.
max_all_labels <- 4

questions_for <- function(issue) {
  q <- trend_questions[trend_questions$issue == issue, ]
  q <- q[order(q$question_order), ]
  setNames(q$series_id, q$title)
}

# Short names for issues in links, e.g. "Issue Ratings" -> "issue-ratings".
issue_slug <- function(x) gsub("[^a-z0-9]+", "-", tolower(x))

# Links to a view look like
#   .../#issue=issue-ratings&q=race_relations&by=Race&g=Black
# This reads one into a list (NULL when the link has no issue in it).
read_trend_link <- function(hash) {
  if (is.null(hash) || !nzchar(hash)) return(NULL)
  parts <- parseQueryString(sub("^#", "", hash))
  issue <- trend_issues[issue_slug(trend_issues) == (parts$issue %||% "")]
  if (length(issue) != 1) return(NULL)
  list(issue = issue, q = parts$q %||% "", by = parts$by %||% "", g = parts$g %||% "")
}
trend_link <- function(issue, q, by, g) {
  enc <- function(x) URLencode(x, reserved = TRUE)
  paste0("#issue=", issue_slug(issue), "&q=", enc(q),
         if (nzchar(by)) paste0("&by=", enc(by), "&g=", enc(g)) else "")
}

list_and <- function(x) {
  if (length(x) < 3) paste(x, collapse = " and ")
  else paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

# ---- UI -----------------------------------------------------------------------

issue_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "intro",
      "Choose an issue and a question, and filter by one group at a time if you like.",
      "Dots sit at the middle of the dates each poll was in the field, on an evenly spaced",
      "timeline, so the gaps between dots show the real time between polls.",
      "Hover over or tap a dot for the details."),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 8)),
      card(class = "trend-controls", card_body(
        selectInput(ns("issue"), "Issue", choices = trend_issues, width = "100%"),
        radioButtons(ns("question"), "Question", choices = questions_for(trend_issues[1]),
                     width = "100%"),
        tags$hr(),
        selectInput(ns("filter_by"), "Filter by",
                    choices = c("No filter (all voters)" = "", trend_filters), width = "100%"),
        conditionalPanel("input.filter_by !== ''", ns = ns,
          selectInput(ns("group"), "Group", choices = NULL, width = "100%")),
        p(class = "trend-hint",
          "One filter at a time: the poll has about 900 respondents,",
          "so groups within groups get too small.")
      )),
      card(card_body(
        uiOutput(ns("header")),
        # Copies the address bar, which always holds a link to the current view.
        tags$button(type = "button", class = "btn btn-link trend-copy",
          onclick = paste0("var b=this; navigator.clipboard.writeText(location.href).then(",
                           "function(){b.textContent='Link copied';",
                           "setTimeout(function(){b.textContent='Copy a link to this graph';},2000);});"),
          "Copy a link to this graph"),
        plotlyOutput(ns("plot"), height = "380px"),
        uiOutput(ns("notes")),
        tags$details(class = "trend-details",
          tags$summary("Question wording and margins of error"),
          uiOutput(ns("wording"))),
        tags$details(class = "trend-details",
          tags$summary("Numbers"),
          tableOutput(ns("numbers")))
      ))
    ),
    tags$footer(class = "app-footer",
      "How these are calculated: percentages are weighted and include everyone who was",
      "asked the question, including people who answered Not sure. Groups follow the poll's",
      "standard definitions; people who did not give an answer for a filter (for example a",
      "party other than the three listed) are counted in all voters only. A point is hidden",
      sprintf("when fewer than %d people in the group were asked.", min_n_shown),
      "Questions are linked across polls only where the poll's own records link them.",
      "The small marks along the bottom of each graph show when every poll was taken.")
  )
}

# ---- Server -------------------------------------------------------------------

# `active` is TRUE while the Issue Trends tab is showing; the link in the
# address bar only follows this tab then.
issue_trends_server <- function(id, active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    # ---- Opening a link to a view -------------------------------------------
    # The link's question and group are applied once the menus have caught up
    # (choosing an issue resets the question list, and a filter its groups).
    pending <- reactiveValues(q = NULL, g = NULL)
    link_read <- reactiveVal(FALSE)
    observeEvent(session$clientData$url_hash, {
      link <- read_trend_link(session$clientData$url_hash)
      if (!is.null(link)) {
        pending$q <- link$q
        pending$g <- link$g
        if (identical(link$issue, input$issue)) {
          if (link$q %in% questions_for(link$issue)) updateRadioButtons(session, "question", selected = link$q)
          pending$q <- NULL
        } else {
          updateSelectInput(session, "issue", selected = link$issue)
        }
        if (link$by %in% trend_filters) updateSelectInput(session, "filter_by", selected = link$by)
      }
      link_read(TRUE)
    }, once = TRUE)

    # A new issue lists its own questions.
    observeEvent(input$issue, {
      ch <- questions_for(input$issue)
      sel <- if (!is.null(pending$q) && pending$q %in% ch) pending$q else ch[1]
      pending$q <- NULL
      updateRadioButtons(session, "question", choices = ch, selected = sel)
    }, ignoreInit = TRUE)

    # A new filter lists its own groups, labeled with the filter's name.
    observeEvent(input$filter_by, {
      if (nzchar(input$filter_by)) {
        g <- trend_groups[trend_groups$filter == input$filter_by, ]
        g <- g$group[order(g$group_order)]
        sel <- if (!is.null(pending$g) && pending$g %in% g) pending$g else g[1]
        pending$g <- NULL
        updateSelectInput(session, "group", label = input$filter_by, choices = g, selected = sel)
      }
    })

    # ---- Keeping the address bar's link up to date ----------------------------
    observe({
      req(link_read(), active(), input$issue, input$question)
      by <- input$filter_by %||% ""
      g  <- if (nzchar(by)) req(input$group) else ""
      req(input$question %in% questions_for(input$issue))
      updateQueryString(trend_link(input$issue, input$question, by, g), mode = "replace")
    })

    # Everything about the chosen question and group.
    chosen <- reactive({
      req(input$issue, input$question)
      q <- trend_questions[trend_questions$issue == input$issue &
                           trend_questions$series_id == input$question, ]
      req(nrow(q) == 1)   # skip the moment between switching issue and its questions
      filter_by <- input$filter_by %||% ""
      group <- if (nzchar(filter_by)) input$group else "All voters"
      req(group, nzchar(filter_by) == FALSE ||
                 group %in% trend_groups$group[trend_groups$filter == filter_by])

      polls <- trend_polls[trend_polls$issue == q$issue & trend_polls$series_id == q$series_id, ] |>
        left_join(select(poll_dates, wave_code, field_label, field_mid), by = "wave_code") |>
        arrange(field_mid)
      res <- trend_results[trend_results$issue == q$issue & trend_results$series_id == q$series_id &
                           trend_results$filter == filter_by & trend_results$group == group, ]
      list(q = q, polls = polls, res = res,
           cats = strsplit(q$categories, "|", fixed = TRUE)[[1]],
           label = if (nzchar(filter_by)) sprintf("%s (%s)", group, tolower(filter_by)) else "All voters")
    })

    # One row per poll: number asked and the percent for each answer.
    by_poll <- reactive({
      ch <- chosen()
      out <- ch$polls[c("wave_code", "wave_name", "field_label", "field_mid")]
      out$n_asked <- ch$res$n_asked[match(out$wave_code, ch$res$wave_code)]
      for (cat in ch$cats) {
        r <- ch$res[ch$res$category == cat, ]
        out[[cat]] <- r$percent[match(out$wave_code, r$wave_code)]
      }
      out
    })

    output$header <- renderUI({
      ch <- chosen()
      waves <- ch$polls$wave_name
      asked <- if (length(waves) == 1) paste("Asked in 1 poll:", waves)
               else if (length(waves) <= 4) sprintf("Asked in %d polls: %s", length(waves), list_and(waves))
               else sprintf("Asked in %d polls, %s to %s", length(waves), waves[1], waves[length(waves)])
      cols <- color_for(ch$cats)
      tagList(
        h2(class = "trend-title", ch$q$title),
        p(class = "trend-showing", paste("Showing:", ch$label)),
        p(class = "trend-asked", asked),
        div(class = "trend-legend",
          lapply(ch$cats, function(cat) span(tags$i(style = paste0("background:", cols[[cat]])), cat)))
      )
    })

    output$plot <- renderPlotly({
      ch <- chosen(); d <- by_poll()
      cols <- color_for(ch$cats)
      n_polls <- nrow(d)

      # The hover box for a poll lists every answer, so it is the same on each line.
      hover <- vapply(seq_len(n_polls), function(i) {
        vals <- vapply(ch$cats, function(cat) {
          v <- d[[cat]][i]; sprintf("%s: %s", cat, if (is.na(v)) "hidden" else paste0(v, "%"))
        }, "")
        paste0("<b>", d$wave_name[i], " poll</b><br>", d$field_label[i], "<br>", ch$label, "<br>",
               paste(vals, collapse = "<br>"), "<br>",
               format(d$n_asked[i], big.mark = ","), " asked")
      }, "")

      p <- plot_ly()
      # A small mark at the bottom for every poll, so the uneven gaps are visible.
      p <- add_trace(p, x = poll_dates$field_mid, y = rep(1, nrow(poll_dates)),
                     type = "scatter", mode = "markers", hoverinfo = "skip", showlegend = FALSE,
                     marker = list(symbol = "line-ns-open", size = 8, color = "#b5b6b4",
                                   line = list(width = 1.5, color = "#b5b6b4")))
      for (cat in ch$cats) {
        y <- d[[cat]]
        shown <- which(!is.na(y))
        labelled <- if (n_polls <= max_all_labels) shown else unique(c(shown[1], shown[length(shown)]))
        text <- ifelse(seq_len(n_polls) %in% labelled, paste0(y, "%"), "")
        # The first poll's label goes on the left, so it never sits on its line.
        pos <- ifelse(n_polls > 1 & seq_len(n_polls) == 1, "middle left", "middle right")
        p <- add_trace(p, x = d$field_mid, y = y, type = "scatter", mode = "lines+markers+text",
                       name = cat, showlegend = FALSE, connectgaps = FALSE,
                       line = list(color = cols[[cat]], width = 2.5),
                       marker = list(color = cols[[cat]], size = 10,
                                     line = list(color = "#ffffff", width = 1.5)),
                       text = text, textposition = pos, cliponaxis = FALSE,
                       textfont = list(size = 13, color = "#1a1a1a"),
                       hovertext = hover, hoverinfo = "text")
      }
      p |>
        layout(
          font = list(family = "Archivo Narrow, Arial Narrow, Arial, sans-serif", size = 13),
          margin = list(l = 44, r = 40, t = 10, b = 36),
          hovermode = "closest",
          hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#C8C9C7",
                            font = list(color = "#1a1a1a", size = 13)),
          # An evenly spaced timeline, one gridline per year. It starts a few
          # months before the first poll to leave room for labels.
          xaxis = list(type = "date", range = c("2019-07-01", "2027-01-01"),
                       tickmode = "array", tickvals = paste0(2020:2026, "-01-01"),
                       ticktext = as.character(2020:2026), ticklabelmode = "period",
                       showgrid = TRUE, gridcolor = "#eeeeee",
                       zeroline = FALSE, fixedrange = TRUE, title = ""),
          yaxis = list(range = c(-2, 104), tickvals = c(0, 25, 50, 75, 100), ticksuffix = "%",
                       gridcolor = "#e6e6e6", zeroline = FALSE, fixedrange = TRUE, title = "")
        ) |>
        config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$notes <- renderUI({
      ch <- chosen(); d <- by_poll()
      hidden <- d[rowSums(!is.na(d[ch$cats])) == 0, ]
      notes <- c(
        if (nrow(hidden)) sprintf("Not shown because fewer than %d people in this group were asked: %s.",
                                  min_n_shown, list_and(sprintf("%s (%d asked)", hidden$wave_name, hidden$n_asked))),
        ch$q$collapse_note, ch$q$pooled_note, ch$q$partial_note)
      notes <- notes[!is.na(notes) & nzchar(notes)]
      tagList(lapply(notes, function(n) p(class = "trend-note", n)))
    })

    output$wording <- renderUI({
      ch <- chosen()
      tagList(
        p(ch$q$wording),
        h3(class = "trend-h3", "Margin of error by poll"),
        tags$ul(class = "trend-moe", lapply(seq_len(nrow(ch$polls)), function(i) {
          tags$li(tags$b(paste0(ch$polls$wave_name[i], ": ")), ch$polls$moe_text[i], " ",
                  span(class = "qid", ch$polls$question_id[i]))
        })),
        p(class = "trend-note",
          "These margins of error are for all voters. They are larger for groups,",
          "and the poll does not publish them for groups.")
      )
    })

    output$numbers <- renderTable({
      ch <- chosen(); d <- by_poll()
      out <- data.frame(Poll = d$wave_name, check.names = FALSE)
      for (cat in ch$cats) out[[cat]] <- ifelse(is.na(d[[cat]]), "Hidden", paste0(d[[cat]], "%"))
      out$Asked <- format(d$n_asked, big.mark = ",")
      names(out)[1] <- ch$label
      out
    }, striped = FALSE, spacing = "xs", align = "l")
  })
}
