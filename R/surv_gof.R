# Goodness-of-Fit hypothesis test for Mode Effect

#' Create an adjustR_gof object
#'
#' @param model A survey GLM object
#' @param topbox_col Name of the top box column
#' @param topbox_value Value indicating the top box in topbox_col
#' @param mode_col Name of the mode column
#' @return An adjustR_gof object
#' @importFrom rlang enquo as_name sym
#' @export

adjustR_gof <- function(model, topbox_col, topbox_value, mode_col) {

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
    class = "adjustR_gof"
  )
  validate_adjustR_gof(result)
  return(result)
}

#' Validate a gof object
#'
#' @param m a adjustR_gof object to validate
#' @return The validated adjustR_gof object
#' Throws an error if the model object is invalid

validate_adjustR_gof <- function(m) {
  # Check that model is a list with the correct class
  if (!is.list(m) || !inherits(m, "adjustR_gof")) {
    stop("Object must be a 'adjustR_gof' object")
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

#' Wrapper Function
#'
#' @description
#' This is a wrapper function to create the adjustR_gof object and implement
#' the Goodness-of-Fit test. It is a public-facing method.
#'
run_gof_test = function(model, topbox_col, topbox_value, mode_col){
  # Capture the expressions
  topbox_col_expr <- rlang::enquo(topbox_col)
  mode_col_expr <- rlang::enquo(mode_col)

  # Convert expressions to strings
  topbox_col_name <- rlang::as_name(topbox_col_expr)
  mode_col_name <- rlang::as_name(mode_col_expr)

  gof_object = adjustR_gof(model, topbox_col_name, topbox_value, mode_col_name)

  gof_object = run_test(gof_object)

  return(gof_object)
}

#' Implement Goodness-of-Fit test
#'
#' @description
#' The main function associated with the Goodness-of-Fit hypothesis test.
#' Implements the adjustR_gof methods to process data, calculate observed
#' test statistics and simulations. Modifies the adjustR_gof object in place
#' and returns the object invisbly.
#' @param m An adjustR_gof object
#'
run_test = function(m, ...){
  UseMethod("run_test")
}

run_test.adjustR_gof = function(m, ...){
  m$cells = generate_post_cells.adjustR_gof(m, ...)

  return(invisible(m))
}

#' Generate post-stratification cells for a adjustR_gof object
#'
#' BIMODAL ----------------------------------------------
#' This function assumes a bimodal analysis. Topbox in this instance is used to
#' split the data into two separate parts, generating cells and topbox cells
#' only once. A multimodal analysis will need to loop over a possible list of
#' topbox values and assign post-stratification cells for each.
#'
#' @param m An adjustR_gof object
#' @param ... Additional Arguments (not used)
#' @return A dataframe of post-stratification cells
generate_post_cells.adjustR_gof <- function(m, ...) {
  data <- m$model$data

  strata <- unique(c(
    all.vars(formula(m$model))[-1],
    sub(".*\\$", "", colnames(m$model$survey.design$strata)) # remove table name if exists
  ))

  topbox_col_sym <- rlang::sym(m$topbox_col)

  topcells <- data %>%
    dplyr::filter(!!topbox_col_sym == m$topbox_value) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata))) %>%
    dplyr::summarize(numerator = dplyr::n())

  cells <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata))) %>%
    dplyr::summarise(n = dplyr::n())

  prop_cells <- topcells %>%
    dplyr::left_join(cells) %>%
    dplyr::mutate(sampProp = numerator / n)

  return(prop_cells)
}

