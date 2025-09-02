#' Cache Dynamic Fit Index (DFI) Cutoffs for CFA Models
#'
#' @description
#' This function is a smart wrapper for the `dynamic` package. It automatically
#' detects the model type (continuous/categorical, single/multi-factor) and calls
#' the appropriate DFI function (`catOne`, `catHB`, `nnorOne`, or `nnorHB`).
#'
#' It automatically generates a filename based on the model object's name
#' (e.g., passing a model object named `my_model` will create `my_model_dfi.Rds`).
#' It then checks for this cached file in the specified directory, loading it if
#' it exists, or running the DFI calculation and saving the result if it does not.
#'
#' @param model A fitted `lavaan` object from the `cfa`/`sem` function.
#' @param cache_dir A string specifying the path to the directory where the cached
#' results file should be saved or loaded from.
#' @param data The empirical dataset. **Required** for continuous models
#' (which use `nnorOne` or `nnorHB`) for bootstrapping.
#' @param reps The number of replications for the simulation. Defaults to 500
#' @param estimator The estimation method. If `NULL` (default), it's automatically
#' detected from the `lavaan` object using `model@Options$estimator.orig`.
#' A user-provided value here will override the automatic detection.
#' @param ... Other arguments to be passed to the dynamic function, like `plot`.
#'
#' @return Dynamic fit index (DFI) cutoffs from the relevant `dynamic` function.
#'
#' @author Lukas Novak
#' @maintainer Lukas Novak <lukasjirinovak@gmail.com>
cache_dynamic <- function(model, cache_dir, data = NULL, reps = 500, robust = TRUE, estimator = NULL, ...) {
  
  # 1. Automatic file path generation
  model_name <- deparse(substitute(model))
  sanitized_model_name <- gsub("[^A-Za-z0-9_.-]", "_", model_name)
  file_name <- paste0(sanitized_model_name, "_dfi.Rds")
  file_path <- file.path(cache_dir, file_name)
  
  # 2. Check for cached file
  if (file.exists(file_path)) {
    message("Found cached result. Loading from: '", file_path, "'")
    return(readRDS(file_path))
  }
  
  # 3. If not cached, proceed with the analysis
  message("No cached result found. Running dynamic function...")
  message("-> Check: Result will be cached at: '", file_path, "'")
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  if (!inherits(model, "lavaan")) {
    stop("This version of cache_dynamic is designed for fitted lavaan objects.")
  }
  
  # 4. Detect model properties
  
  # --- MINIMAL FIX STARTS HERE ---
  # The original check for `is_categorical` was not robust.
  # The most reliable way to know if a model is categorical is to check if it
  # contains thresholds ('|' operator) in its parameter table.
  is_categorical <- any(model@ParTable$op == "|")
  # --- MINIMAL FIX ENDS HERE ---
  
  num_factors <- model@pta$nfac[[1]]
  
  dynamic_function_name <- if (is_categorical) {
    if (num_factors == 1) "catOne" else "catHB"
  } else {
    if (num_factors == 1) "nnorOne" else "nnorHB"
  }
  
  if (!exists(dynamic_function_name, mode = "function")) {
    stop("Required function '", dynamic_function_name, "' not found. Is the `dynamic` package loaded?", call. = FALSE)
  }
  dynamic_function <- get(dynamic_function_name)
  
  message("-> Check: Model detected as ", ifelse(is_categorical, "Categorical", "Continuous"), " with ",
          num_factors, " factor(s). Calling `", dynamic_function_name, "()`.")
  
  # 5. Simplified and Correct Estimator Detection
  final_estimator <- NULL
  if (!is.null(estimator)) {
    final_estimator <- toupper(estimator)
    message("-> Check: Using user-specified estimator '", final_estimator, "'.")
  } else {
    final_estimator <- toupper(model@Options$estimator.orig)
    message("-> Check: Automatically detected estimator '", final_estimator, "' from lavaan object.")
  }
  
  # 6. Argument Handling and Execution
  args_list <- list(model = model, reps = reps, estimator = final_estimator, ...)
  
  # Add robust argument only for functions that support it (catHB and nnorHB)
  if (dynamic_function_name %in% c("catHB", "nnorHB")) {
    args_list$robust <- robust
  }  
  
  # The 'data' argument is ONLY for non-categorical (continuous) models.
  if (!is_categorical) {
    if (is.null(data)) {
      stop("`", dynamic_function_name, "` requires a dataset. Please provide it via the `data` argument.", call. = FALSE)
    }
    args_list$data <- data
  }
  
  result <- tryCatch(
    do.call(dynamic_function, args_list),
    error = function(e) stop("Error during execution of '", dynamic_function_name, "':\n", e$message, call. = FALSE)
  )
  
  # 7. Save and return
  saveRDS(result, file = file_path)
  message("... Successfully cached result to '", file_path, "'.")
  return(result)
}