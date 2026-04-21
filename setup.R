if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}

library("rstudioapi")

SOURCE_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
LIB_FOLDER <- file.path(SOURCE_FOLDER, "libraries")
RESULTS_FOLDER <- file.path(SOURCE_FOLDER, "results")
TESTS_FOLDER <- file.path(SOURCE_FOLDER, "tests")

setwd(SOURCE_FOLDER)

if (!dir.exists(RESULTS_FOLDER)) dir.create(RESULTS_FOLDER)
if (!dir.exists(TESTS_FOLDER)) dir.create(TESTS_FOLDER)

results <- list()
