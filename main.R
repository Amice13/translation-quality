if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}

library("rstudioapi")

SOURCE_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(SOURCE_FOLDER)

source("setup.R")
source("dependencies.R")
source("config.R")
source("data.R")
source("functions.R")
source("output.R")
source("tests.R")
