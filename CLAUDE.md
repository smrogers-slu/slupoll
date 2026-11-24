# CLAUDE.md: SLU/YouGov Poll Dashboard

R Shiny dashboard for the SLU/YouGov Poll (Missouri voters, 11 waves, June 2020 to August 2026), modeled on the CES dashboards. Owner: Steve Rogers (poll director, not a programmer). Day-to-day work: Aditi (student RA) with Claude. Hosting: Posit Connect Cloud, published by Aditi (she said on November 10, 2026 that publishing is her job; the handoff had said Steve would set it up). Claude prepares the files but does not deploy. Targets: first live version January 2027, update February 2027.

## Where things are

- **Reference library (read-only):** the shared Dropbox folder `Dashboard/RA Reference Library`. On Aditi's Mac: `~/Dropbox/Dashboard/RA Reference Library`. Start with its `README.md` (RULES FOR CLAUDE), then the guides in `03 Guides` in the order the README gives.
- **CES example apps (read-only):** `Dashboard/CES Files`.
- **Working notes:** `Dashboard/Aditi Working`: `OPEN ITEMS for Steve.md` (assumptions and questions; Steve replies there) and `PROGRESS LOG.md`. Read both at the start of every session and update both at the end.
- **This repo:** keep it outside Dropbox. It is currently **public** on GitHub.

## Rules (from the handoff and the library README)

1. Never write to, move, rename or delete anything in `RA Reference Library` or `CES Files`. Copy into the repo or working folder first.
2. The library is the only source on how the poll works. If it is silent or contradictory, record it in the open items file; do not guess.
3. Data: only the public de-identified files in `01 Data`. Never try to recreate zip code, birth year or exact age.
4. Never put passwords, API keys, tokens or anything identifying in code, the repo or any file.
5. Work independently. Ask Aditi only when truly blocked or the choice is big and hard to undo, in plain language with a recommendation. Teach as you go; she is newer to R and to the poll.
6. No em dashes in anything written for Steve or Aditi.

## Poll conventions (details in `03 Guides/05 Analysis Conventions.md`)

- Include all 11 waves. Key waves by wave code (`STLU0001` ... `STLU0016`); display the wave name ("August 2026").
- February 2026: use the reweighted file (the only one in the library) and its `weight`.
- Percentages weighted with `weight`; thresholds use unweighted counts. Hide a chart point unless at least **31** unweighted respondents were asked.
- "Not sure" stays in the base and counts as non-support. Drop only `skipped` and `not asked`, identified by value label, not code.
- Combine categories from unrounded shares, then round once. Numeric/slider items: weighted mean and weighted median; no margin of error for medians.
- Community type = `HRSA_Classification`. Race groups per `04 Demographic Groups.md`: White, Black, All non-white (`race` 1, 2, not 1); no separate Hispanic group (too small). Whether Black is shown separately is an open item for Steve.
- Variable names are NOT stable across waves. Never match questions by name alone; Steve is building the crosswalk.
- Read `06 Data Oddities and Known Issues.md` before using any variable (e.g. February 2025 `EDU001` kids-in-school looks wrong; August 2026 `EDU002` has placeholder labels).
- Wording comes from the Master Codebook. Style: SLU design system (`08 Style`); no Brandon Grotesque font; never blue next to red on political charts; no orange.

## Repo layout

- `apps/question-search/`: the Question Search app (`app.R`, `data/question_catalog.csv`, `manifest.json`). Self-contained for deployment. It is unbranded for now (Aditi, November 14, 2026): the page shows the placeholder in `poll_name` at the top of `app.R` and no logo.
- `branding/`: the SLU/YouGov Poll logo, kept out of the app folder so it is not published while the app is unbranded.
- `apps/issue-graphs/index.html`: all issues on one static page with an Issue menu (each issue has its own link, `#issue=<slug>`). This is the one to publish: Connect Cloud would not let Aditi pick the repo a second time (November 23, 2026). Built by `data-raw/issue-graphs/build_all_issue_graphs.R`.
- `apps/issue-graphs/<issue>/index.html`: one static page per issue (Abortion, Issue Ratings) with trend graphs, a question list and a one-at-a-time demographic filter. Published on Connect Cloud as a "Static Document" (no manifest needed). Generated; do not edit by hand.
- `data-raw/issue-graphs/`: `build_issue_graphs.R` (computes the results and writes the page), `issue_graphs_template.html` (the page's look) and `series - <Issue>.csv` (which question ID goes in which graph in each poll, with the library source for each cross-poll link). Run from the repo root: `Rscript data-raw/issue-graphs/build_all_issue_graphs.R` (every issue plus the combined page). To add an issue, add its series file and its name in that script. Full results (including groups under 31) and checks go to `Aditi Working/Issue Graphs`, never into this public repo.
- `manifest.json` tells Posit Connect Cloud which files and R packages the app needs. Regenerate it whenever the app's files or packages change: `rsconnect::writeManifest("apps/question-search", appPrimaryDoc = "app.R")`.
- `data-raw/build_question_results.R`: computes each question's overall weighted result (the chart in the search app's drop-down) from the public `.sav` files. Writes `question_results.csv` and `question_results_notes.csv` (why a question has no chart) into the app's `data/`. Run it after the catalog script.
- `data-raw/build_published_graphs.R`: lists the questions that have a published graph (from the Graph Information files) with a link to that wave's results page on the poll website. The library has no per-graph web addresses.
- `data-raw/build_question_catalog.R`: rebuilds the catalog from the Master Codebook. Run from the repo root after the codebook changes. Set `SLUPOLL_LIBRARY` if the library is not at the default Dropbox path.

Run the search app locally from the repo root: `shiny::runApp("apps/question-search")`.
