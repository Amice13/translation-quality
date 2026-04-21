LIB_FOLDER <- paste0(SOURCE_FOLDER, "/libraries")
RESULTS_FOLDER <- paste0(SOURCE_FOLDER, "/results")

# List of libraries to include
dependencies <- c(
  "ggdendro",
  "quanteda.textstats",
  "quanteda",
  "irr",
  "lme4",
  "psych",
  "ggplot2",
  "tidyverse",
  "reshape2",
  "betareg",
  "memisc",
  "dplyr",
  "report",
  "nsyllable",
  "testthat"
)

install_dependencies <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) {
    install.packages(missing, destdir = LIB_FOLDER)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

install_dependencies(dependencies)

# Store session data
writeLines(capture.output(sessionInfo()), "r-session.txt")
session <- sessionInfo()
save(session, file = "r-session.Rdata")

# Save citations
writeLines(
  unlist(
    lapply(
      names(sessionInfo()[["otherPkgs"]]),
      function(x) tryCatch(toBibtex(citation(x)), error = function(e) NULL)
    )
  ),
  "./results/citations.bib"
)

rm(
  install_dependencies,
  session,
  dependencies,
  LIB_FOLDER
)
