# Make sure you have these packages listed in your package's DESCRIPTION file under "Imports"
#' @importFrom dplyr mutate bind_rows select slice rename_with if_else case_when group_by ungroup n row_number all_of filter lead lag cur_group_id any_of
#' @importFrom tibble tibble add_column as_tibble
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr map_df map reduce
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names

#' Create a Formatted Summary Table from 'dynamic' Package Objects
#'
#' @description
#' This function aggregates one or more DFI objects from the 'dynamic' package
#' into a single, publication-ready tibble.
#'
#' The function automatically formats the output in one of two ways:
#' - **Long Format (<= 3 models):** Models are stacked vertically. For cleaner
#'   presentation in this format, each model's name is displayed only once.
#' - **Wide Format (> 3 models):** Models are presented side-by-side to create a
#'   compact and easily comparable table. This format is triggered automatically
#'   when four or more models are provided.
#'
#' In simple terms, the 'Magnitude' of misspecification indicates the
#' **size of the simulated error** for each DFI level (e.g., the standardized
#' value of a missing cross-loading that was used in the simulation to generate
#' that cutoff).
#'
#' @param models A character vector of DFI object names created by functions
#'   like `dynamic::cfaHB()` or `dynamic::cfaOne()` (e.g., `c("DFI_model_one", "DFI_model_two")`).
#' @param show_magnitude A logical value. If `TRUE` (the default), the 'Magnitude'
#'   column is included in the output table. Set to `FALSE` to hide it.
#' @param show_fitted_model A logical value. If `TRUE` (the default), the
#'   'Fitted Model' row containing the empirical results from your data is
#'   included. Set to `FALSE` to show only the DFI cutoff levels.
#' @param add_separator_rows A logical value. If `TRUE`, a blank row is added
#'   between each model's data block for visual separation. In wide format, this
#'   separates the wrapped blocks of models. Defaults to `FALSE`.
#' @param add_interpretation A logical value. If `TRUE`, adds an 'Interpretation'
#'   column that provides a qualitative label for the fit of the user's model.
#'   **Warning:** This feature uses fixed, conventional cutoffs (Hu & Bentler, 1999)
#'   and is provided for comparative purposes only. The primary purpose of the
#'   'dynamic' package is to *avoid* relying on such generalized rules. The DFI
#'   cutoffs generated for your specific model are the most appropriate benchmark.
#' @param remove_duplicated_model_names A logical value. If `TRUE` (long format only),
#'   removes duplicated model names for cleaner presentation. If `FALSE`, it will
#'   keep duplicated model names for further processing. Defaults to `TRUE`.
#' @param models_per_row An integer specifying how many models to display side-by-side
#'   in wide format before wrapping to a new row. Defaults to `3`. Only applies
#'   when there are more than 3 models.
#'
#' @return A formatted tibble (`tbl_df`) summarizing the DFI results for all
#'   provided models, ready for reporting.
#'
#' @references
#' Hu, L. T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
#' covariance structure analysis: Conventional criteria versus new alternatives.
#' *Structural Equation Modeling: A Multidisciplinary Journal, 6*(1), 1-55.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This is a conceptual example. First, create DFI objects:
#' # DFI_A <- dynamic::cfaHB(fit_object_A)
#' # DFI_B <- dynamic::cfaHB(fit_object_B)
#' # DFI_C <- dynamic::cfaHB(fit_object_C)
#' # DFI_D <- dynamic::cfaHB(fit_object_D)
#'
#' # --- LONG FORMAT EXAMPLES ---
#' # Get the default table for two models
#' # create_dynamic_table(c("DFI_A", "DFI_B"))
#'
#' # Add a separator row and interpretation
#' # create_dynamic_table(c("DFI_A", "DFI_B"), add_separator_rows = TRUE, add_interpretation = TRUE)
#'
#' # --- WIDE FORMAT EXAMPLES ---
#' # With four models, the function automatically switches to wide format
#' # create_dynamic_table(c("DFI_A", "DFI_B", "DFI_C", "DFI_D"))
#'
#' # Control wrapping by setting models_per_row
#' # create_dynamic_table(c("DFI_A", "DFI_B", "DFI_C", "DFI_D"), models_per_row = 2)
#'
#' # Add a separator between the wrapped rows of models
#' # create_dynamic_table(
#' #   c("DFI_A", "DFI_B", "DFI_C", "DFI_D"),
#' #   models_per_row = 2,
#' #   add_separator_rows = TRUE
#' # )
#' }
create_dynamic_table <- function(models,
                                 show_magnitude = TRUE,
                                 show_fitted_model = TRUE,
                                 add_separator_rows = FALSE,
                                 remove_duplicated_model_names = TRUE,
                                 add_interpretation = FALSE,
                                 models_per_row = 3) {
  
  # --- Input Validation ---
  if (!is.character(models)) {
    stop("`models` must be a character vector of object names, e.g., c('DFI_ONE', 'DFI_TWO').")
  }
  if (length(models) == 0) {
    stop("At least one model object name must be provided in the `models` vector.")
  }
  
  # Retrieve the actual objects from the environment
  dfi_objects <- mget(models, envir = parent.frame())
  model_names <- models
  num_models <- length(dfi_objects)
  
  # --- Helper function to process each DFI object ---
  process_dfi_object <- function(dfi_object, model_name) {
    cutoffs_matrix <- dfi_object$cutoffs
    cutoffs_rownames <- rownames(cutoffs_matrix)
    level_rows <- which(stringr::str_detect(cutoffs_rownames, "^Level-"))
    
    # Gracefully handle missing Magnitude column
    magnitude_col <- if ("Magnitude" %in% colnames(cutoffs_matrix)) {
      as.character(cutoffs_matrix[level_rows, "Magnitude"])
    } else {
      rep(NA_character_, length(level_rows))
    }
    
    cutoffs_clean <- tibble::tibble(
      Misspecification = cutoffs_rownames[level_rows],
      SRMR = as.numeric(cutoffs_matrix[level_rows, "SRMR"]),
      RMSEA = as.numeric(cutoffs_matrix[level_rows, "RMSEA"]),
      CFI = as.numeric(cutoffs_matrix[level_rows, "CFI"]),
      Magnitude = magnitude_col
    )
    
    empirical_fit_clean <- dfi_object$fit %>%
      dplyr::rename_with(stringr::str_trim) %>%
      dplyr::select(dplyr::any_of(c("SRMR", "RMSEA", "CFI"))) %>%
      dplyr::mutate(
        Misspecification = "Fitted Model",
        Magnitude = NA_character_,
        .before = 1
      )
    
    dplyr::bind_rows(cutoffs_clean, empirical_fit_clean) %>%
      dplyr::mutate(Model = stringr::str_remove(model_name, "^DFI_"), .before = 1)
  }
  
  # --- Process all DFI objects into one single, raw table ---
  raw_table <- purrr::map_df(seq_along(dfi_objects), ~ process_dfi_object(dfi_objects[[.x]], model_names[.x]))
  
  # --- APPLY FORMATTING AND OPTIONS ---
  
  # 1. Conditionally filter out the 'Fitted Model' row
  if (!show_fitted_model) {
    raw_table <- dplyr::filter(raw_table, Misspecification != "Fitted Model")
  }
  
  # 2. Conditionally add the Interpretation column
  if (add_interpretation) {
    if (!show_fitted_model) {
      warning("`add_interpretation` is TRUE, but `show_fitted_model` is FALSE. Cannot add interpretation without the fitted model row. Ignoring.")
      raw_table$Interpretation <- NA_character_
    } else {
      raw_table <- raw_table %>%
        dplyr::mutate(
          Interpretation = dplyr::if_else(
            Misspecification == "Fitted Model",
            dplyr::case_when(
              RMSEA <= 0.06 & CFI >= 0.95 & SRMR <= 0.08 ~ "Good",
              RMSEA <= 0.08 & CFI >= 0.90 & SRMR <= 0.10 ~ "Acceptable",
              TRUE ~ "Poor"
            ),
            NA_character_
          )
        )
    }
  }
  
  # 3. Conditionally remove the Magnitude column
  if (!show_magnitude) {
    raw_table <- dplyr::select(raw_table, -Magnitude)
  }
  
  # --- DECIDE TABLE FORMAT (LONG vs. WIDE) ---
  
  # Use long format for 3 or fewer models
  if (num_models <= 3) {
    final_table <- raw_table %>%
      dplyr::group_by(Model) %>%
      dplyr::group_modify(~ {
        if (add_separator_rows && dplyr::cur_group_id() < num_models) {
          dplyr::bind_rows(.x, .x[NA, ][1, ])
        } else {
          .x
        }
      }) %>%
      dplyr::ungroup()
    
    if (remove_duplicated_model_names) {
      final_table <- final_table %>%
        dplyr::mutate(
          Model = dplyr::if_else(is.na(dplyr::lag(Model)) | dplyr::lag(Model) != Model, Model, "")
        )
    }
    return(final_table)
  }
  
  # Use wide format for more than 3 models
  if (num_models > 3) {
    
    # Split models into chunks for wrapping
    model_chunks <- split(unique(raw_table$Model), ceiling(seq_along(unique(raw_table$Model)) / models_per_row))
    
    all_blocks <- list() # To store each formatted tibble block
    
    for (i in seq_along(model_chunks)) {
      
      chunk_models <- model_chunks[[i]]
      
      # Prepare the data for the current chunk
      chunk_data <- raw_table %>%
        dplyr::filter(Model %in% chunk_models) %>%
        # Ensure consistent factor levels for ordering
        dplyr::mutate(Model = factor(Model, levels = chunk_models)) %>%
        tidyr::pivot_wider(
          names_from = Model,
          values_from = -c(Model, Misspecification)
        )
      
      # --- Build Header Rows ---
      metrics <- setdiff(names(raw_table), c("Model", "Misspecification"))
      
      # Add empty separator columns between models
      final_chunk_data <- chunk_data %>%
        # Add a blank column before each model's data (except the first)
        purrr::reduce(rev(chunk_models)[-length(chunk_models)], function(data, model_name) {
          first_col_name <- paste0(metrics[1], "_", model_name)
          data %>% tibble::add_column(!!paste0("sep_", model_name) := "", .before = first_col_name)
        }, .init = .)
      
      # Rebuild headers to match new structure with separators
      model_header_vec_sep <- purrr::map(chunk_models, ~ c(paste("Model", .x), rep("", length(metrics))))
      metric_header_vec_sep <- purrr::map(chunk_models, ~ c("", metrics))
      
      # Interleave with empty strings for the separator columns
      interleaved_model_header <- unlist(purrr::reduce(
        model_header_vec_sep[-1], ~ c(.x, "", .y), .init = model_header_vec_sep[[1]]
      ))
      
      interleaved_metric_header <- unlist(purrr::reduce(
        metric_header_vec_sep[-1], ~ c(.x, "", .y), .init = metric_header_vec_sep[[1]]
      ))
      
      # Create final header tibbles
      model_header_final <- rlang::set_names(
        as.list(c("Misspecification", interleaved_model_header)),
        names(final_chunk_data)
      ) %>% tibble::as_tibble()
      
      metric_header_final <- rlang::set_names(
        as.list(c("", interleaved_metric_header)),
        names(final_chunk_data)
      ) %>% tibble::as_tibble()
      
      # Set column names of data to match headers for binding
      names(final_chunk_data) <- names(model_header_final)
      
      # Combine headers and data
      block <- dplyr::bind_rows(model_header_final, metric_header_final, final_chunk_data)
      all_blocks[[i]] <- block
    }
    
    # Combine all model blocks vertically
    final_table <- purrr::reduce(seq_along(all_blocks), function(acc, i) {
      if (i == 1) return(all_blocks[[i]])
      
      # Create a separator row with correct width
      separator_row <- rlang::set_names(
        as.list(rep("", ncol(all_blocks[[i]]))),
        names(all_blocks[[i]])
      ) %>% tibble::as_tibble()
      
      if (add_separator_rows) {
        dplyr::bind_rows(acc, separator_row, all_blocks[[i]])
      } else {
        dplyr::bind_rows(acc, all_blocks[[i]])
      }
    }, .init = NULL)
    
    return(final_table)
  }
}