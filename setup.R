if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}

library("rstudioapi")

SOURCE_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
LIB_FOLDER <- paste0(SOURCE_FOLDER, "/libraries")
RESULTS_FOLDER <- paste0(SOURCE_FOLDER, "/results")
TESTS_FOLDER <- paste0(SOURCE_FOLDER, "/tests")

setwd(SOURCE_FOLDER)

ifelse(!dir.exists(file.path(RESULTS_FOLDER)),
       dir.create(file.path(RESULTS_FOLDER)),
       "Directory Exists")

ifelse(!dir.exists(file.path(TESTS_FOLDER)),
       dir.create(file.path(TESTS_FOLDER)),
       "Directory Exists")
