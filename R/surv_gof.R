# Goodness-of-Fit hypothesis test for Mode Effect

#' Note to self -- implement a join-by argument for the generate post_cells
#' function. Should pass implicitly.

#' Wrapper Function
#'
#' @param model A survey GLM object
#' @param topbox_col Name of the top box column
#' @param topbox_value Value indicating the top box in topbox_col
#' @param mode_col Name of the mode column
#' @description
#' This is a user friendly wrapper function that translates inputs to the expected
#' S3 object "adjustR_gof" and calculates Goodness-of-Fit over that object.
#'
run_GOF <- function(model, topbox_col, topbox_value, mode_col) {
  # Capture the expressions
  topbox_col_expr <- rlang::enquo(topbox_col)
  mode_col_expr <- rlang::enquo(mode_col)

  # Convert expressions to strings
  topbox_col_name <- rlang::as_name(topbox_col_expr)
  mode_col_name <- rlang::as_name(mode_col_expr)

  gof_object <- construct_GOF(model, topbox_col_name, topbox_value, mode_col_name)

  gof_object <- .calculate_GOF.gof(gof_object)

  return(gof_object)
}

#' Implement Goodness-of-Fit test
#'
#' @description
#' The main function associated with the Goodness-of-Fit hypothesis test.
#' Implements the adjustR_gof methods to process data, calculate observed
#' test statistics and simulations. Modifies the adjustR_gof object in place
#' and returns the object invisibly.
#' @param m An adjustR_gof object
#'
.calculate_GOF.gof <- function(m, ...) {
  m$cells <- .generate_post_cells.gof(m, ...)

  return(invisible(m))
}

#' Object Constructor for adjustR_gof
#'
#' @param model A survey GLM object
#' @param topbox_col Name of the top box column
#' @param topbox_value Value indicating the top box in topbox_col
#' @param mode_col Name of the mode column
#' @return An adjustR_gof object
#' @importFrom rlang enquo as_name sym
#' @export

construct_GOF <- function(model, topbox_col, topbox_value, mode_col) {
  result <- structure(
    list(
      model = model,
      topbox_col = topbox_col,
      topbox_value = topbox_value,
      mode_col = mode_col,
      cells = NULL,
      matched_cells = NULL,
      null_distribution = NULL,
      simulations = NULL
    ),
    class = "gof"
  )
  .validate.gof(result)
  return(result)
}

#' Validate a gof object
#'
#' @param m a adjustR_gof object to validate
#' @return The validated adjustR_gof object
#' Throws an error if the model object is invalid

.validate.gof <- function(m) {
  # Check that model is a list with the correct class
  if (!is.list(m) || !inherits(m, "gof")) {
    stop("Object must be a 'gof' object")
  }

  # Check that all the necessary fields are present
  required_fields <- c("model", "topbox_col", "topbox_value", "mode_col")
  missing_fields <- setdiff(required_fields, names(m))
  if (length(missing_fields) > 0) {
    stop("Missing required fields ", paste(missing_fields, collaps = ", "))
  }

  # Check that model is valid survey object
  if (!inherits(m$model, "svyglm")) {
    stop("Model must be a survey glm object")
  }

  # Check that topbox and mode cols are in data:
  model_data <- m$model$data
  if (!m$topbox_col %in% names(model_data)) {
    stop("'topbox_col not found in survey data")
  }
  if (!m$mode_col %in% names(model_data)) {
    stop("'mode_col not found in survey data")
  }

  # Check that topbox_value is present in topbox column
  if (!m$topbox_value %in% model_data[[m$topbox_col]]) {
    stop("'topbox_value' not found in 'topbox_col'")
  }

  # Check that mode column has at least 2 values, warn if more
  if (length(unique(model_data[, m$mode_col])) < 2) {
    stop("'mode_col' must have at least two unique values")
  }
  # Warn if multi-modal analysis is attempted
  if (length(unique(model_data[, m$mode_col])) > 2) {
    warning("Function does not yet support multimodal analyses")
  }
  invisible(NULL)
}

#' Generate post-stratification cells for a adjustR_gof object
#'
#' BIMODAL ----------------------------------------------
#' This function assumes a bimodal analysis. Topbox in this instance is used to
#' split the data into two separate parts, generating cells and topbox cells
#' only once. A multimodal analysis will need to loop over a possible list of
#' topbox values and assign post-stratification cells for each.
#'
#' @description
#' Generates the post-stratification cells based on the provided survey model's
#' formula and the survey design stratification; e.g., using
#' \code{svyglm(response ~ gender + race, ...)} with the associated design
#' \code{svydesign(..., strata = ~region, ...)} will form cells based on the
#' combination of gender, race, and region. Sample proportions are generated
#' using the observed number of "topbox" responses in the \eqn{m}th survey mode,
#' the \eqn{j}th post-stratification cell, and the \eqn{i}th iteration (or year),
#' defined collectively as \eqn{x_{mij}}, and the sample size for the mode, cell,
#' and year, defined \eqn{n_{mij}}. The sample proportion of a given cell, mode,
#' and year is given \eqn{\frac{x_mij}{n_mij}}.
#'
#' @param m An adjustR_gof object
#' @param ... Additional Arguments (not used)
#' @return A list of tibbles
.generate_post_cells.gof <- function(m, strata, ...) {
  data <- m$model$data

  prop_modes <- list()
  mode_values <- unique(data[[m$mode_col]])
  mode_sym <- rlang::sym(m$mode_col)

  strata <- unique(c(
    all.vars(formula(m$model))[-1], # remove response var from formula
    sub(".*\\$", "", colnames(m$model$survey.design$strata)) # remove table name if exists
  ))

  # Generate post-stratifcation cells
  topbox_col_sym <- rlang::sym(m$topbox_col)

  topcells <- data %>%
    dplyr::filter(!!topbox_col_sym == m$topbox_value) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata))) %>%
    dplyr::summarize(numerator = dplyr::n(), .groups = "keep")

  cells <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "keep")

  prop_cells <- topcells %>%
    dplyr::left_join(cells) %>%
    dplyr::mutate(sampProp = numerator / n)

  # Calculate Sample Proportions
  for (i in mode_values) {
    filtered <- prop_cells %>%
      dplyr::filter(all(!!mode_sym == i)) %>%
      dplyr::mutate(
        !!rlang::sym(paste0("sampProp_", i)) := sampProp,
        !!rlang::sym(paste0("n_", i)) := n,
        !!rlang::sym(paste0("x_", i)) := numerator
      ) %>%
      dplyr::select(dplyr::all_of(strata), dplyr::matches(paste0("_", i)))

    prop_modes[[paste0("mode_", i)]] <- filtered[, !names(filtered) == mode_sym]
  }
  return(prop_modes)
}

match_cells.gof = function(m, ...){
  strata <- unique(c(
    all.vars(formula(m$model))[-1], # remove response var from formula
    sub(".*\\$", "", colnames(m$model$survey.design$strata)) # remove table name if exists
  ))
  prop_cells = Reduce(function(x, y) merge(x, y, .by = strata, all = TRUE), m$cells) # merge all at once
  data.table::setDT(prop_cells)

  samp_cols = grep("^sampProp_", names(prop_cells), value = TRUE)
  n_cols = grep("^n_", names(prop_cells), value = TRUE)
  x_cols = grep("^x_", names(prop_cells), value = TRUE)


}
