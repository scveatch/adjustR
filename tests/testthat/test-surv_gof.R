# Create sample data
create_sample_data <- function(n = 20) {
  set.seed(123) # for reproducibility
  data <- data.frame(
    id = 1:n,
    weight = stats::runif(n, 0.5, 1.5),
    stratum = sample(1:5, n, replace = TRUE),
    topbox = sample(c("Yes", "No"), n, replace = TRUE),
    mode = sample(c("Phone", "Web"), n, replace = TRUE),
    gender = sample(c("M", "F"), n, replace = TRUE),
    response = rbinom(n, 1, 0.7)
  )

  design <- survey::svydesign(
    ids = ~data$id,
    weights = ~data$weight,
    strata = ~data$stratum,
    data = data
  )

  model <- survey::svyglm(
    response ~ topbox + mode + gender, design = design, family = quasibinomial()
  )

  return(list(data = data, design = design, model = model))
}

sample_data <- create_sample_data()

test_that("adjustR_gof constructer works properly", {
  gof_obj <- tryCatch({
    adjustR_gof(sample_data$model, topbox, "Yes", mode)
  }, error = function(e) {
    fail(paste("Error creating gof_obj:", e$message))
    return(NULL)
  })

  expect_false(is.null(gof_obj), "gof_obj was not created successfully")

  if (!is.null(gof_obj)) {
    expect_s3_class(gof_obj, "adjustR_gof")
    expect_named(gof_obj, c("model", "topbox_col", "topbox_value", "mode_col",
                            "cells", "matched_cells", "null_distribution", "simulations"))
    expect_equal(gof_obj$topbox_col, "topbox")
    expect_equal(gof_obj$mode_col, "mode")
  }
})
