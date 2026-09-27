pkgs <- c("haven", "dplyr", "tidyr", "purrr", "readr", "ggplot2", "ragg", "rdrobust", "rddensity", "RDHonest", "sandwich", "lmtest")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
invisible(lapply(c(pkgs, "grid"), library, character.only = TRUE))

font <- "Times New Roman"
walk(c("output/figures", "output/tables", "output/csv"), dir.create, recursive = TRUE, showWarnings = FALSE)

source("R/01_data.R")
source("R/02_functions.R")
source("R/03_analysis.R")
source("R/04_figures.R")
source("R/05_tables.R")