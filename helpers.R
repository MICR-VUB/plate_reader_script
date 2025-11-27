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

#' Identify replicates with unique maximum or minimum RFUc values
#'
#' For each value of \code{inducer_conc} and \code{effector_conc}, this function
#' determines which replicate has the highest and lowest \code{RFUc}.
#' A replicate scores a "maximum" count for a combination of \code{inducer_conc}
#' and \code{effector_conc} values if it has the unique highest \code{RFUc}
#' among all replicates. Similarly, it scores a "minimum" count if it has the
#' unique lowest value. Ties are ignored. The input data must contain only one
#' value for \code{strain}, \code{inducer_ID} and \code{effector_ID}.
#'
#' @param data A data frame containing at least the columns
#'   \code{strain}, \code{inducer_ID}, \code{inducer_conc}, \code{effector_ID},
#'   \code{effector_conc}, \code{replicate} and \code{RFUc}.
#'   \code{replicate} is coerced to a factor if not already one.
#'
#' @return A tibble with one row for \code{"maximum"} and one for
#'   \code{"minimum"}, and one column per replicate. Each cell gives the
#'   number groups for which that replicate was the unique maximum or minimum.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble(
#'   strain         = "WT",
#'   inducer_ID     = "Ara",
#'   effector_ID    = "As",
#'   inducer_conc   = c(0, 0, 10, 10, 50, 50),
#'   effector_conc  = c(0, 0, 0, 0, 0, 0),
#'   replicate      = c(1, 2, 1, 2, 1, 2),
#'   RFUc           = c(100, 120, 200, 180, 250, 300)
#' )
#'
#' unique_max_or_min(df)
#' }
#' @export
unique_max_or_min <- function(data) {
  # Check required columns.
  required <- c("strain", "inducer_ID", "inducer_conc", "effector_ID", "effector_conc", "replicate", "RFUc")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  
  # Check that strain, inducer_ID and effector_ID are constant.
  if (n_distinct(data$strain) != 1) stop("`strain` must contain exactly one value.")
  if (n_distinct(data$inducer_ID) != 1) stop("`inducer_ID` must contain exactly one value.")
  if (n_distinct(data$effector_ID) != 1) stop("`effector_ID` must contain exactly one value.")
  
  # Which replicate is at max or min?
  data$replicate <- as.factor(data$replicate)
  scoring_df <- data |>
    group_by(inducer_conc, effector_conc) |>
    summarise(
      max_val = max(RFUc, na.rm = TRUE),
      min_val = min(RFUc, na.rm = TRUE),
      reps_at_max = list(replicate[RFUc == max_val]),
      reps_at_min = list(replicate[RFUc == min_val])
    ) |>
    ungroup() |>
    mutate(
      # Exclude occurrences where two replicates are equal.
      unique_max = ifelse(lengths(reps_at_max) == 1, as.character(reps_at_max), NA),
      unique_min = ifelse(lengths(reps_at_min) == 1, as.character(reps_at_min), NA)
    ) |>
    select(inducer_conc, effector_conc, unique_max, unique_min)
  
  # Count occurrences per replicate.
  counts <- scoring_df |>
    pivot_longer(
      cols = c(unique_max, unique_min),
      names_to = "type",
      values_to = "replicate"
    ) |>
    filter(!is.na(replicate)) |>
    group_by(type, replicate) |>
    summarise(n = n()) |>
    ungroup()
  
  # Convert to tidy output
  out <- counts |>
    pivot_wider(names_from = replicate, values_from = n, values_fill = 0) |>
    mutate(type = recode(type, unique_max = "maximum", unique_min = "minimum"))
  
  out
}