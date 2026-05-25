LIB_FOLDER <- file.path(SOURCE_FOLDER, "libraries")
RESULTS_FOLDER <- file.path(SOURCE_FOLDER, "results")
TESTS_FOLDER <- file.path(SOURCE_FOLDER, "tests")


if (!dir.exists(RESULTS_FOLDER)) dir.create(RESULTS_FOLDER)
if (!dir.exists(TESTS_FOLDER)) dir.create(TESTS_FOLDER)

results <- list()
