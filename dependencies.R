LIB_FOLDER <- file.path(SOURCE_FOLDER, "libraries")

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
  "dplyr",
  "betareg",
  "memisc",
  "report",
  "nsyllable",
  "testthat"
)

install_dependencies <- function(pkgs, lib = LIB_FOLDER) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) {
    install.packages(missing, destdir = lib)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

install_dependencies(dependencies)

session <- sessionInfo()
writeLines(capture.output(session), "r-session.txt")
save(session, file = "r-session.Rdata")

writeLines(
  unlist(
    lapply(
      names(session[["otherPkgs"]]),
      function(x) tryCatch(toBibtex(citation(x)), error = function(e) NULL)
    )
  ),
  file.path(RESULTS_FOLDER, "citations.bib")
)

rm(install_dependencies, session, dependencies, LIB_FOLDER)
