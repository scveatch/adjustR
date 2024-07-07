#' Helper Functions

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

# Setup Goodness-of-Fit object
create_gof_object <- function() {
  sample_data <- create_sample_data()
  tryCatch({
    adjustR_gof(sample_data$model, topbox, "Yes", mode)
  }, error = function(e) {
    stop(paste("Error creating gof_obj:", e$message))
  })
}

create_gof_and_data = function() {
  sample_data = create_sample_data()
  tryCatch({
    adjustR_gof(sample_data$model, topbox, "Yes", mode)
    list(
      data = sample_data$data

    )
  }, error = function(e){
    stop(paste("Error creating gof_obj:", e$message))
  })
}
