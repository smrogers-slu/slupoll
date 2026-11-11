# slupoll

Interactive dashboard for the SLU/YouGov Poll, a nonpartisan survey of Missouri voters run by Saint Louis University with YouGov.

## Apps

| App | Folder | Status |
|---|---|---|
| Question Search | `apps/question-search` | First version, runs locally |

## Running the Question Search app

Requires R with the `shiny`, `bslib`, `DT` and `dplyr` packages. From this folder:

```r
shiny::runApp("apps/question-search")
```

## Publishing on Posit Connect Cloud

Connect Cloud publishes straight from this GitHub repo. In Connect Cloud, choose **Publish > Shiny**, pick this repository and branch, and set the primary file to `apps/question-search/app.R`. The `manifest.json` in that folder lists the packages to install. If you change the app's files or packages, regenerate it first:

```r
rsconnect::writeManifest("apps/question-search", appPrimaryDoc = "app.R")
```

## Updating the question list

The app reads `apps/question-search/data/question_catalog.csv`, which is built from the poll's Master Codebook:

```sh
Rscript data-raw/build_question_catalog.R
```
