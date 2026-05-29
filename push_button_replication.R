# Make sure run from the root of the project directory
if (!file.exists("_targets.R")) {
  stop("Run this script from the project root, where _targets.R is located.", call. = FALSE)
}

if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop(
      "This project uses renv, but the renv package is not installed.\n",
      "Please install it first by running:\n\n",
      "install.packages('renv')\n\n",
      "Then rerun this script.",
      call. = FALSE
    )
  }
  
  renv::restore(prompt = FALSE)
}

targets::tar_make()