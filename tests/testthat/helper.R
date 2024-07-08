#' Helper Functions

# Create sample data
create_sample_data <- function(n = 20) {

  data = readRDS(test_path("ODOT_test_small.rds"))

  design <- survey::svydesign(
    ids = ~data$id,
    weights = ~data$weight,
    strata = ~data$region,
    data = data,
    nest = TRUE,
    fpc = ~data$size
  )

  model <- survey::svyglm(
    topbox ~ factor(mode) + factor(gender) + factor(race) + factor(year),
    family = quasibinomial(), design = design
  )

  return(list(data = data, design = design, model = model))
}

# Setup Goodness-of-Fit object
create_gof_object <- function() {
  sample_data <- create_sample_data()
  tryCatch({
    adjustR_gof(sample_data$model, "topbox", 1, "mode")
  }, error = function(e) {
    stop(paste("Error creating gof_obj:", e$message))
  })
}

create_data_object = function(){
  sample_data = create_sample_data()
  tryCatch({
    list(
      data = sample_data$data,
      model = sample_data$model
    )
  }, error = function(e){
    stop(paste("Error creating data:"), e$message)
  })
}
