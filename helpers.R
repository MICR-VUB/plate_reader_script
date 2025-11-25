library(dplyr)
library(ggplot2)

#' Add hours since first time point
#'
#' Adds a new column `time_h` to a data frame, representing the number of
#'   hours passed since the first value in the `Time` column.
#'
#' @param data A data frame with a POSIXct column named `Time`.
#'
#' @return A data frame with an additional numeric column `time_h`.
#'
#' @examples
#' data <- data.frame(Time = as.POSIXct(c("2024-01-01 08:00", "2024-01-01 10:30")))
#' format_time(data)
#'
format_time <- function(data) {
  data$time_h <- as.numeric(difftime(data$Time, data$Time[1], units = "hours"))
  data
}

#' Reorder factor levels in a column based on numeric order
#'
#' Reorders the levels of a factor column in a data frame based on increasing
#'   numeric value. The column must already be a factor with numeric-like levels
#'   (e.g., "1", "10", "100"). Returns a modified copy of the input data frame
#'   with the reordered factor.
#'
#' @param data A data frame.
#' @param column_name A string giving the name of the factor column to reorder.
#'
#' @return A data frame.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   dose = factor(c("10", "2", "100", "1"))
#' )
#' df <- reorder_factor_column(df, "dose")
#' levels(df$dose)
reorder_factor_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    cli::cli_abort(c(
      "{.arg column_name} must be the name of a column in {.arg data}.",
      "x" = "You've tried to set {column_name} as {.arg column_name}."
    ))
  }
  
  factor_column <- data[[column_name]]
  if (!is.factor(factor_column)) {
    cli::cli_abort(c(
      "{.arg column_name} must be a factor.",
      "x" = "You've tried to set {column_name} as {.arg column_name}.",
      "x" = "{column_name} is a {class(column_name)}."
    ))
  }
  
  sorted_levels <- sort(as.numeric(levels(factor_column)))
  data[[column_name]] <- factor(
    factor_column,
    levels = as.character(sorted_levels),
    ordered = TRUE
  )
  
  data
}

#' Subtract background signal from medium control
#'
#' This function subtracts the mean OD and fluorescence (FL) values
#'   of the "medium" control from each sample at corresponding time points.
#'   It also calculates the relative fluorescence units for the uncorrected
#'   OD and FL (`RFU` column) and the corrected OD and FL (`RFUc` column).
#'
#' @param data A data frame containing at least the columns `strain`, `time`,
#'   `OD`, and `FL`.
#'
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' corrected_data <- subtract_medium(data)
#' }
subtract_medium <- function(data) {
  med <- data |>
    filter(strain == "medium") |>
    group_by(time) |>
    summarise(
      OD_med_t = mean(OD, na.rm = TRUE),
      FL_med_t = mean(FL, na.rm = TRUE)
    ) |>
    ungroup()
  
  data |>
    left_join(med, by = "time") |>
    mutate(
      ODc = OD - OD_med_t,
      FLc = FL - FL_med_t,
      RFU = FL / OD,
      RFUc = FLc / ODc
    ) |>
    select(-OD_med_t, -FL_med_t)
}

#' Create a ggplot for a specific response variable
#'
#' This function generates a scatter plot of the specified response variable
#'   against time, colour-coded by a categorical variable
#'
#' @param data A data frame containing the columns `time`, the response
#'   variable, and the categorical variable specified by `cat_var`.
#' @param yvar A string specifying the name of the response variable to plot
#'   (e.g. "ODc", "FLc", or "RFU").
#' @param cat_var A string specifying the name of the categorical
#'   variable to colour-code the data by.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' make_plot(data, "ODc")
#' }
make_plot <- function(data, yvar, cat_var) {
  if (!yvar %in% names(data)) {
    cli::cli_abort(c(
      "{.arg yvar} must be the name of a column in {.arg data}.",
      "x" = "You've tried to set {yvar} as {.arg yvar}."
    ))
  }
  if (!cat_var %in% names(data)) {
    cli::cli_abort(c(
      "{.arg cat_var} must be the name of a column in {.arg data}.",
      "x" = "You've tried to set {cat_var} as {.arg cat_var}."
    ))
  }
  
  ggplot(data, aes(x = time, y = .data[[yvar]], color = .data[[cat_var]])) +
    geom_point(cex = 1)
}

#' Plot multiple replicates separately with three response variables each
#'
#' This function filters the data per replicate and creates three plots
#'   (`ODc`, `FLc`, and `RFU` vs. time, colour-coded by `cat_var`) for each,
#'   arranged in a grid. The function loops over the specified number of
#'   replicates and displays the plots with a title.
#'
#' @param data A data frame containing at least the columns `replicate`, `time`,
#'   `ODc`, `FLc`, `RFU`, and the categorical variable specified by `cat_var`.
#' @param num_replicates An integer specifying the number of replicates to plot.
#' @param cat_var A string specifying the name of the categorical
#'   variable to colour-code the data by.
#'
#' @return This function is called for its side effects: it displays plots for
#'   each replicate. It also returns `data` invisibly.
#'
#' @examples
#' \dontrun{
#' plot_replicates_separately(data, 4)
#' }
plot_replicates_separately <- function(data, num_replicates, cat_var) {
  for (i in 1:num_replicates) {
    data_loop <- filter(data, replicate == i)
    plots <- lapply(
      c("ODc", "FLc", "RFU"),
      make_plot,
      data = data_loop,
      cat_var = cat_var
    )
    grid <- patchwork::wrap_plots(plots, ncol = 2) +
      plot_annotation(
        title = paste("Replicate", i),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
    print(grid)
  }
  invisible(data)
}