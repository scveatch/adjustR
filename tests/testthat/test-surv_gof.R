#' Fixtures
#' ---------------------------------------------------------

gof_fixture <- function(test) {
  # Generate fresh object
  gof_obj <- create_gof_object()

  # Test
  test(gof_obj)

  # Cleanup
  rm(gof_obj)
}

data_fixture <- function(test) {
  objs <- create_data_object()

  test(objs)

  rm(objs)
}

post_strat_fixture <- function(test) {
  objs <- check_post_strat()

  test(objs)

  rm(objs)
}

#' Object Tests
#' ---------------------------------------------------------

test_that("adjustR_gof constructer works properly", {
  gof_fixture(function(gof_obj) {
    expect_false(is.null(gof_obj), "gof_obj was not created successfully")

    if (!is.null(gof_obj)) {
      expect_s3_class(gof_obj, "gof")
      expect_named(gof_obj, c(
        "model", "topbox_col", "topbox_value", "mode_col",
        "cells", "matched_cells", "null_distribution", "simulations"
      ))
      expect_equal(gof_obj$topbox_col, "topbox")
      expect_equal(gof_obj$mode_col, "mode")
    }
  })
})

test_that("validate gof object returns proper errors", {
  gof_fixture(function(gof_obj) {
    expect_null(.validate.gof(gof_obj))

    invalid <- gof_obj
    invalid$topbox_col <- "non-existent-col"
    expect_error(.validate.gof(invalid), "'topbox_col not found in survey data")
  })

  gof_fixture(function(gof_obj) {
    invalid2 <- gof_obj
    invalid2$topbox_value <- "non-existent-value"
    expect_error(.validate.gof(invalid2), "'topbox_value' not found in 'topbox_col'")
  })

  gof_fixture(function(gof_obj) {
    invalid3 <- gof_obj
    invalid3$mode_col <- "non-existent-col"
    expect_error(.validate.gof(invalid3), "'mode_col not found in survey data")
  })
})

#' Methods Tests
#' ---------------------------------------------------------
test_that("Generate Post Stratification functions", {
  post_strat_fixture(function(objs) {
    test_df <- .generate_post_cells.gof(objs$model)
    expect_equal(sum(test_df$sampProp), sum(objs$correct$sampProp))
  })
})
