#' Creates two plots to compare the prediction of the GPP for two data sets, typically the train- and the test-set
#'
#' @param mod is the model
#' @param df_train is the data frame of the train data
#' @param df_test ist the data frame of the test data
#' @param title_train is the title string for the first plot
#' @param title_test is the titlte string for the second plot
#'
#' @return Graphic object with the two plot

eval_model <- function(mod, df_train, df_test, title_train = "Training set", title_test = "Test set"){

  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  suppressWarnings( # Suppress warnings from Box-Cox-Transformation
    df_train$predicted <- predict(mod, newdata = df_train)
  )

  df_test <- df_test |>
    drop_na()
  suppressWarnings( # Suppress warnings from Box-Cox-Transformation
    df_test$predicted <- predict(mod, newdata = df_test)
  )

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(GPP_NT_VUT_REF, predicted)

  metrics_test <- df_test |>
    yardstick::metrics(GPP_NT_VUT_REF, predicted)

  # extract values from metrics tables
  rmse_train <- metrics_train |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_train <- metrics_train |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  rmse_test <- metrics_test |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_test <- metrics_test |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(x=GPP_NT_VUT_REF, y=predicted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", linewidth=1.1, color = "red") +
    geom_abline(slope = 1, intercept = 0, linewidth=1.2, color = "blue") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = title_train) +
    theme_classic()

  plot_2 <- ggplot(data = df_test, aes(x=GPP_NT_VUT_REF, y=predicted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", linewidth=1.1, color = "red") +
    geom_abline(slope = 1, intercept = 0, linewidth=1.2, color = "blue") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = title_test) +
    theme_classic()

  out <- cowplot::plot_grid(plot_1, plot_2)

  return(out)
}
