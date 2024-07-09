#' Helper Functions

# Create sample data
create_sample_data <- function() {
  data <- readRDS(test_path("ODOT_test_small.rds"))

  design <- survey::svydesign(
    ids = ~ data$id,
    weights = ~ data$weight,
    strata = ~ data$region,
    data = data,
    nest = TRUE,
    fpc = ~ data$size
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
  tryCatch(
    {
      construct_GOF(sample_data$model, "topbox", 1, "mode")
    },
    error = function(e) {
      stop(paste("Error creating gof_obj:", e$message))
    }
  )
}


# Check function for Post-strat cells:
check_post_strat <- function() {
  sample_data <- create_sample_data()

  tryCatch(
    {
      # Construct Objs
      gof_obj <- construct_GOF(sample_data$model, "topbox", 1, "mode")
      data <- sample_data$data

      # Construct post-strat cells
      topcells <- data %>%
        dplyr::filter(topbox == 1) %>%
        dplyr::group_by(year, region, gender, race, mode) %>%
        dplyr::summarise(numerator = dplyr::n())

      cells <- data %>%
        dplyr::group_by(year, region, gender, race, mode) %>%
        dplyr::summarise(n = dplyr::n())

      ## join
      propCells <- topcells %>%
        dplyr::left_join(cells) %>%
        dplyr::mutate(sampProp = numerator / n)

      # Calculate sample proportions
      propTel<-propCells%>%
        filter(Mode==1)%>%
        mutate(sampProp1=sampProp,
               n1=n, N1=N,
               x1=numerator)%>%
        select(Year, REGION, GENDER, RACEc,n1, N1, sampProp1, x1)
      head(propTel)

      ## Mail
      propMail<-propCells%>%
        filter(Mode==2)%>%
        mutate(sampProp2=sampProp,
               n2=n, N2=N, x2=numerator)%>%
        select(Year, REGION, GENDER, RACEc,n2, N2, sampProp2, x2)
      head(propMail)

      ## Cell Match
      propCells<-propTel%>%
        left_join(propMail)%>%
        mutate(dij=sampProp1-sampProp2)%>%
        mutate(logit1=log(sampProp1/(1-sampProp1)),
               logit2=log(sampProp2/(1-sampProp2)))%>%
        mutate(gij=logit1-logit2)%>%
        mutate(strat=paste("(",Year,",", REGION,",",GENDER,",",RACEc,")",sep=""))
    },
    error = function(e) {
      stop(paste("Error in check function"), e$message)
    }
  )

  list(
    correct = propCells,
    model = gof_obj
  )
}
