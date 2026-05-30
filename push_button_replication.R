# Make sure run from the root of the project directory
if (!file.exists("_targets.R")) {
  stop("Run this script from the project root, where _targets.R is located.", call. = FALSE)
}

# Make sure same R environment
renv::restore()
# I had to run the following code in Posit Cloud directory
# update.packages(ask = FALSE, checkBuilt = TRUE)

targets::tar_make()