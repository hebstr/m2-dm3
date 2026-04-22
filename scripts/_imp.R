.init <- mice(df_setup, maxit = 0)

.method <- .init$method
.method[.miss_data$variable] <- "rf"
.method["tt_death"] <- ""

.predictors <- .init$predictorMatrix
.predictors[, "tt_death"] <- 0

df_imp <-
  df_setup |>
  mice(
    m = 10,
    maxit = 5,
    predictorMatrix = .predictors,
    method = .method
  ) |>
  complete() |>
  as_tibble()

# save(df_imp, file = "data/df_imp.rdata")
