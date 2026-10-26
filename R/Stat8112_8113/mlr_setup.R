# Run scripts from AIXZ/R/Stat811_8113, or source them from any directory.
# Resolve data relative to this file, never download data or install packages.
mlr_source <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(mlr_source)) {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  mlr_source <- if (length(arg)) sub("^--file=", "", arg[1]) else NULL
}
mlr_script_dir <- if (!is.null(mlr_source)) dirname(normalizePath(mlr_source)) else getwd()
mlr_root <- normalizePath(file.path(mlr_script_dir, "../.."))
mlr_data <- file.path(mlr_root, "data")
mlr_output <- file.path(mlr_script_dir, "output")
dir.create(mlr_output, showWarnings = FALSE)
options(contrasts = c("contr.treatment", "contr.poly"))
