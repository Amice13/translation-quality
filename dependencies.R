DIST_DIRECTORY <- paste0(SOURCE_FOLDER, "/libraries")

# List of libraries to include
dependencies <- c(
  "ggplot2",
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
  "nsyllable"
)

install_dependencies <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) {
    install.packages(missing, destdir = DIST_DIRECTORY)
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
      function(x) toBibtex(citation(x))
    )
  ),
  "./results/citations.bib"
)

rm(
  install_dependencies,
  session,
  dependencies,
  DIST_DIRECTORY
)
