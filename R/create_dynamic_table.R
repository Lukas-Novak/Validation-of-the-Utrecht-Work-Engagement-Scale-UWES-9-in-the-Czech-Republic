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
                                 models_per_row = 3,
                                 bold_fitted_model = FALSE) {
  
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
    
    # --- FIX 1: Add validation to safely skip malformed or NULL objects ---
    if (is.null(dfi_object) || is.null(dfi_object$cutoffs) || is.null(dfi_object$fit)) {
      warning(paste("Skipping invalid or incomplete DFI object:", model_name), call. = FALSE)
      return(NULL) # Return NULL to be ignored by purrr::map_df
    }
    # --- END FIX ---
    
    cutoffs_matrix <- dfi_object$cutoffs
    cutoffs_rownames <- rownames(cutoffs_matrix)
    level_rows <- which(stringr::str_detect(cutoffs_rownames, "^Level-"))
    
    magnitude_col <- if ("Magnitude" %in% colnames(cutoffs_matrix)) {
      as.character(cutoffs_matrix[level_rows, "Magnitude"])
    } else {
      rep(NA_character_, length(level_rows))
    }
    
    cutoffs_clean <- tibble::tibble(
      Misspecification = cutoffs_rownames[level_rows],
      SRMR = as.character(cutoffs_matrix[level_rows, "SRMR"]),
      RMSEA = as.character(cutoffs_matrix[level_rows, "RMSEA"]),
      CFI = as.character(cutoffs_matrix[level_rows, "CFI"]),
      Magnitude = magnitude_col
    )
    
    empirical_fit_clean <- dfi_object$fit %>%
      dplyr::rename_with(stringr::str_trim) %>%
      dplyr::select(dplyr::any_of(c("SRMR", "RMSEA", "CFI"))) %>%
      dplyr::mutate(
        across(c("SRMR", "RMSEA", "CFI"), as.character),
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
  if (!show_fitted_model) {
    raw_table <- dplyr::filter(raw_table, Misspecification != "Fitted Model")
  }
  if (add_interpretation) {
    if (!show_fitted_model) {
      warning("`add_interpretation` is TRUE, but `show_fitted_model` is FALSE. Ignoring.")
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
  if (bold_fitted_model && show_fitted_model) {
    raw_table <- raw_table %>%
      dplyr::mutate(
        across(-c(Model, Misspecification), ~ dplyr::case_when(
          Misspecification == "Fitted Model" & !is.na(.x) & .x != "" ~ paste0("**", .x, "**"),
          TRUE ~ as.character(.)
        ))
      )
  }
  if (!show_magnitude) {
    raw_table <- dplyr::select(raw_table, -Magnitude)
  }
  
  # --- DECIDE TABLE FORMAT (LONG vs. WIDE) ---
  if (num_models <= 3) {
    return(raw_table)
  }
  
  # --- START: ROBUST WIDE FORMAT LOGIC ---
  if (num_models > 3) {
    model_chunks <- split(unique(raw_table$Model), ceiling(seq_along(unique(raw_table$Model)) / models_per_row))
    all_blocks <- list()
    metrics <- setdiff(names(raw_table), c("Model", "Misspecification"))
    
    for (i in seq_along(model_chunks)) {
      chunk_models <- model_chunks[[i]]
      
      list_of_model_tibbles <- purrr::map(seq_along(chunk_models), function(j) {
        model_name <- chunk_models[j]
        raw_table %>%
          dplyr::filter(Model == model_name) %>%
          dplyr::select(Misspecification, all_of(metrics)) %>%
          dplyr::rename_with(~ paste0(., "_pos_", j), .cols = all_of(metrics))
      })
      
      unsorted_chunk_data <- purrr::reduce(
        list_of_model_tibbles,
        dplyr::full_join,
        by = "Misspecification"
      )
      
      chunk_data <- unsorted_chunk_data %>%
        dplyr::mutate(
          sort_key = dplyr::case_when(
            stringr::str_detect(Misspecification, "^Level-") ~ as.integer(stringr::str_extract(Misspecification, "\\d+")),
            Misspecification == "Fitted Model" ~ 999L,
            TRUE ~ 1000L
          )
        ) %>%
        dplyr::arrange(sort_key) %>%
        dplyr::select(-sort_key)
      
      col_names <- "Misspecification"
      # --- FIX 2: Swap header labels to shift "Misspecification" down one row ---
      header1_labels <- ""
      header2_labels <- "Misspecification"
      # --- END FIX ---
      
      for (j in 1:models_per_row) {
        if (j > 1) {
          col_names <- c(col_names, paste0("sep_col_", j))
          header1_labels <- c(header1_labels, "")
          header2_labels <- c(header2_labels, "")
        }
        col_names <- c(col_names, paste0(metrics, "_pos_", j))
        model_name_label <- if (j <= length(chunk_models)) paste("Model", chunk_models[j]) else ""
        metric_labels <- if (j <= length(chunk_models)) metrics else rep("", length(metrics))
        header1_labels <- c(header1_labels, model_name_label, rep("", length(metrics) - 1))
        header2_labels <- c(header2_labels, metric_labels)
      }
      
      block_data <- tibble::tibble(Misspecification = chunk_data$Misspecification)
      for (j in 1:models_per_row) {
        if (j > 1) {
          block_data[[paste0("sep_col_", j)]] <- ""
        }
        for (metric in metrics) {
          col_name <- paste0(metric, "_pos_", j)
          block_data[[col_name]] <- if (col_name %in% names(chunk_data)) chunk_data[[col_name]] else NA_character_
        }
      }
      
      model_header_final <- rlang::set_names(as.list(header1_labels), col_names) %>% tibble::as_tibble()
      metric_header_final <- rlang::set_names(as.list(header2_labels), col_names) %>% tibble::as_tibble()
      names(block_data) <- col_names
      
      all_blocks[[i]] <- dplyr::bind_rows(model_header_final, metric_header_final, block_data)
    }
    
    final_table <- purrr::reduce(seq_along(all_blocks), function(acc, i) {
      if (i == 1) return(all_blocks[[i]])
      separator_row <- rlang::set_names(as.list(rep("", ncol(all_blocks[[i]]))), names(all_blocks[[i]])) %>% tibble::as_tibble()
      if (add_separator_rows) dplyr::bind_rows(acc, separator_row, all_blocks[[i]]) else dplyr::bind_rows(acc, all_blocks[[i]])
    }, .init = NULL)
    
    return(final_table)
  }
}