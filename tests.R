
RESULTS_FILE <- "results.rds"

# Check that the results object exists
results_exist <- exists("results", envir = .GlobalEnv)

if (!results_exist) {
  stop("The bariable with results does not exist")
}

# Check if this is the generation of tests
test_file_path <- paste0(TESTS_FOLDER, "/", RESULTS_FILE)
test_file_exists <- file.exists(test_file_path)

if (!test_file_exists) {
  saveRDS(results, test_file_path)
  stop("Test file is generated. No testing is required.")
}

test_results <- readRDS(test_file_path)

compare_objects <- function(x, y, tol = 1e-6) {
  if (inherits(x, "lmerMod")) {
    expect_equal(lme4::fixef(x), lme4::fixef(y), tolerance = tol)
    expect_equal(lme4::ranef(x), lme4::ranef(y), tolerance = tol)
    expect_equal(stats::sigma(x), stats::sigma(y), tolerance = tol)
    return()
  }

  if (is.data.frame(x)) {
    expect_equal(normalize_df(x), normalize_df(y))
    return()
  }

  if (is.atomic(x) && !is.numeric(x)) {
    expect_identical(x, y)
    return()
  }

  if (is.numeric(x)) {
    expect_equal(x, y, tolerance = tol)
    return()
  }

  if (is.list(x)) {
    expect_equal(names(x), names(y))
    for (n in names(x)) {
      compare_objects(x[[n]], y[[n]], tol)
    }
    return()
  }
  expect_equal(x, y, tolerance = tol, ignore_attr = TRUE)
}

for (name in names(results)) {
  test_that(paste(name, "does not match"), {
    compare_objects(results[[name]], test_results[[name]])
  })
}

rm(test_results)
