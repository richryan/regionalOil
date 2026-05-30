# Make sure run from the root of the project directory
if (!file.exists("_targets.R")) {
  stop("Run this script from the project root, where _targets.R is located.", call. = FALSE)
}

# Make sure same R environment
renv::restore()
# I had to run the following code in Posit Cloud environment
# update.packages(ask = FALSE, checkBuilt = TRUE)

start_time <- Sys.time()

targets::tar_make()

stop_time <- Sys.time()

difftime(stop_time, start_time, units = "secs")
difftime(stop_time, start_time, units = "mins")
difftime(stop_time, start_time, units = "hours")