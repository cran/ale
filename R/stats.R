## calc_stats.R
#
# Calculate statistical measures based on ALE
#

# ALE statistics -------------

#' Calculate statistics from ALE y values.
#'
#' The following statistics are calculated based on a vector of ALE y values:
#'
#' * ALE deviation (ALED)
#' * ALE range (ALER): range from minimum value of any ALE `y` to the maximum value of any `y`. This is a very simple indication of the dispersion in ALE `y` values.
#' * Normalized ALE deviation (NALED)
#' * Normalized ALE range (NALER)
#'
#' Note that if any ALE `y` values are missing, they will be deleted from the calculation (with their corresponding bin_n).
#'
#' @noRd
#'
#' @param y numeric. Vector of ALE y values.
#' @param bin_n numeric. Vector of counts of rows in each ALE bin. Must be the same length as `y`.
#' @param y_vals numeric. Entire vector of y values. Needed for normalization. If not provided, ale_y_norm_fun must be provided.
#' @param ale_y_norm_fun function. Result of `create_ale_y_norm_function()`. If not provided, `y_vals` must be provided. `calc_stats()` could be faster if `ale_y_norm_fun` is provided, especially in bootstrap workflows that call the same function many, many times.
#' @param x_type character(1). Datatype of the x variable on which the ALE y is based. Values are the result of `var_type()`. Used to determine how to correctly calculate ALE, so if the value is not the default `"numeric"`, then it must be set correctly.
#'
#' @returns Named numeric vector:
#' * aled: ALE deviation (ALED)
#' * aler_min: Minimum (lower value) of the ALE range (ALER)
#' * aler_max: Maximum (upper value) of the ALE range (ALER)
#' * naled: Normalized ALE deviation (ALED)
#' * naler_min: Normalized minimum (lower value) of the ALE range (ALER)
#' * naler_max: Normalized maximum (upper value) of the ALE range (ALER)
#'
calc_stats <- function(
    y,
    bin_n,
    y_vals = NULL,
    ale_y_norm_fun = NULL,
    x_type = 'numeric'
) {

  ## Validate data -------------

  validate(
    !(is.null(y_vals) && is.null(ale_y_norm_fun)),
    msg = cli_alert_danger('Either {.arg y_vals} or {.arg ale_y_norm_fun} must be provided.')
  )

  y <- as.vector(y)  # flatten from 1D matrix inputs to vector

  # Remove any NA y values (perhaps from bootstrapping) and corresponding bin_n
  na_y <- is.na(y)
  y <- y[!na_y]
  bin_n <- bin_n[!na_y]


  ## Prepare internally used functions and data ---------

  # ALED formula.
  # Internal function because it will be reused for both ALED and NALED.
  aled_score <- function(y, n) {
    (y * n) |>
      abs() |>
      sum() |>
      (`/`)(sum(n))
  }

  # Normalized scores
  if (is.null(ale_y_norm_fun)) {
    ale_y_norm_fun <- create_ale_y_norm_function(y_vals)
  }


  ## Calculate the statistics ------------

  # ALER and NALER: minimum negative and positive effects in units of y
  aler <- c(min(y), max(y))

  # Normalized y for NALER
  norm_y <- ale_y_norm_fun(y)

  # Scale of NALER is -50 to +50, representing lowest and highest percentile deviations from the median
  naler <- c(
    min(norm_y),
    max(norm_y)
  )


  # ALED and NALED: Average effect in units of y

  # Create versions of y and bin_n for ALED because the originals might be changed for numeric x.
  aled_y <- y
  aled_bin_n <- bin_n

  # For numeric x, transform the ALE x bin borders to actual bins
  if (x_type == 'numeric') {
    # Set the bin ALE y to the midpoint of the interval borders
    aled_y <- (aled_y[-length(aled_y)] + aled_y[-1]) / 2

    # Add the minimum points (aled_bin_n[1]) to the second bin and then delete the first bin
    aled_bin_n[2] <- aled_bin_n[2] + aled_bin_n[1]
    aled_bin_n <- aled_bin_n[-1]
  }

  aled <- aled_score(aled_y, aled_bin_n)

  # NALED scale is 0 to 100, representing equivalent average percentile effect
  naled <- aled_score(
    ale_y_norm_fun(aled_y),
    aled_bin_n
  )


  ## Return ----------

  return(c(
    aled = aled,
    aler_min = aler[1],
    aler_max = aler[2],
    naled = naled,
    naler_min = naler[1],
    naler_max = naler[2]
  ))
}  # calc_stats()


#' Calculate statistics from 2D ALE y values.
#'
#' When calculating second-order (2D) ALE statistics, there is no difficulty if both variables are categorical. The regular formulas for ALE operate normally. However, if one or both variables is numeric, the calculation is complicated by the necessity to determine the ALE midpoints between the ALE bin ceilings of the numeric variables. This function calculates these ALE midpoints for the numeric variables and resets the ALE bins to these values. The ALE values for ordinal variables are not changed. As part of the adjustment, the lowest numeric bin is merged into the second: the ALE values are completely deleted (since they do not represent a midpoint) and their counts are added to the first true bin.
#'
#' After these possible adjustments, the ALE y values and bin counts are passed to [calc_stats()], which calculates their statistics as an ordinal variable since the numeric variables have thus been discretized.
#'
#' @noRd
#'
#'
#' @param ale_data dataframe. ALE data
#' @param x_cols character. Names of the x columns in `ale_data`.
#' @param x_types character same length as `x_cols`. Variable types (output of var_type()) of corresponding `x_cols`.
#' @param y_vals See documentation for [calc_stats()]
#' @param ale_y_norm_fun See documentation for [calc_stats()]
# @param zeroed_ale See documentation for [calc_stats()]
#'
#' @returns Same as [calc_stats()].
#'
calc_stats_2D <- function(
    ale_data,
    x_cols,
    x_types,
    y_vals = NULL,
    ale_y_norm_fun = NULL
) {
  # ale_data=boot_summary

  if ('numeric' %notin% x_types) {
    # No need to transform anything since the order of records does not matter for ALE statistics for ordinal variables.
    ale_y <- ale_data$.y
    ale_n <- ale_data$.n
  }

  else {
    # Convert ale_data to arrays
    ale_y_ray <-
      paste0(
        '.y ~ ',
        paste0(x_cols, collapse = ' + ')
      ) |>
      stats::as.formula() |>
      stats::xtabs(ale_data)
    ale_n_ray <-
      paste0(
        '.n ~ ',
        paste0(x_cols, collapse = ' + ')
      ) |>
      stats::as.formula() |>
      stats::xtabs(ale_data)

    # The second term of the subtraction below will have values 1, x_lo:x_hi. The values of x_lo and x_hi depend on whether x is numeric.
    if (x_types[1] == 'numeric') {
      x1_lo <- 1
      x1_hi <- nrow(ale_y_ray) - 1
    } else {
      x1_lo <- min(nrow(ale_y_ray), 2)
      x1_hi <- nrow(ale_y_ray)
    }

    if (x_types[2] == 'numeric') {
      x2_lo <- 1
      x2_hi <- ncol(ale_y_ray) - 1
    } else {
      x2_lo <- min(ncol(ale_y_ray), 2)
      x2_hi <- ncol(ale_y_ray)
    }

    # Calculate the midpoint values
    mid_ale_y_ray <-
      (ale_y_ray + ale_y_ray[
        c(1, x1_lo:x1_hi),
        c(1, x2_lo:x2_hi)
        ]) / 2

    # Delete the minimum for numeric variables; they are not midpoints.
    # Shift their counts to the adjacent rows or columns.
    if (x_types[1] == 'numeric') {
      mid_ale_y_ray <- mid_ale_y_ray[-1, , drop = FALSE]
      ale_n_ray[2, ] <- ale_n_ray[2, ] + ale_n_ray[1, ]
      ale_n_ray <- ale_n_ray[-1, , drop = FALSE]
    }
    if (x_types[2] == 'numeric') {
      mid_ale_y_ray <- mid_ale_y_ray[, -1, drop = FALSE]
      ale_n_ray[, 2] <- ale_n_ray[, 2] + ale_n_ray[, 1]
      ale_n_ray <- ale_n_ray[, -1, drop = FALSE]
    }

    ale_y <- mid_ale_y_ray
    ale_n <- ale_n_ray
  }

  return(calc_stats(
    y = ale_y,
    bin_n = ale_n,
    y_vals = y_vals,
    ale_y_norm_fun = ale_y_norm_fun,
    # Now ALE stats can be calculated as ordinal ALE since all the necessary preprocessing has been done.
    x_type = 'ordered'
  ))
}  # calc_stats_2D()


# Create a function that normalizes ALE y values
create_ale_y_norm_function <- function(y_vals) {
  # Centre y_vals on the median.
  # This code works even if y_vals is already centred on the median;
  # it might be slightly off if y_vals is centred on the mean.
  centred_y <- y_vals - stats::median(y_vals)

  # Find the values right below and right above median y (0 for centred_y)

  # Value right below the median
  pre_median  <- if (median(centred_y) != max(centred_y)) {
    max(centred_y[centred_y < 0])
  } else {
    0
  }
  # Value right above the median
  post_median <- if (median(centred_y) != min(centred_y)) {
    min(centred_y[centred_y > 0])
  } else {
    0
  }

  return(
    function(ale_y) {
      # Assign each ALE y value to its respective norm_ale_y (normalized half percentile).
      # ale_y == 0 is assigned at the 50th percentile.
      norm_ale_y <- case_when(
        # When ale_y is between the values right below and above the median (0), normalize it to 0.
        (ale_y >= pre_median) & (ale_y <= post_median) ~ 0,
        # percentiles of the lower half of the y values (0 to 50%)
        # Note: the median is included in both halves.
        ale_y < 0  ~ -stats::ecdf(-1 * (centred_y[centred_y <= 0]))(-ale_y) / 2,
        # The exact median.
        # Normally, the first condition should catch this case, but just in case...
        ale_y == 0 ~ 0,
        # Percentiles of the upper half of the y values (50 to 100%).
        # Note: the median is included in both halves.
        ale_y > 0  ~ stats::ecdf(centred_y[centred_y >= 0])(ale_y) / 2,
      )

      return(norm_ale_y * 100)
    }
  )

}  # create_ale_y_norm_function()



# Other statistics functions -------------


# Provide a vector of descriptive statistics
var_summary <- function(
    var_name,
    var_vals,
    ...,
    p_dist = NULL,
    aler_alpha = c(0.01, 0.05)
) {
  if (!is.null(p_dist)) {
    rand_stats <- p_dist@rand_stats
  }

  # Convert vector to matrix
  if (!is.matrix(var_vals)) {
    var_vals <- as.matrix(var_vals, ncol = 1)  # nocov
  }

  s <-
    var_vals |>
    apply(MARGIN = 2, \(it.col) {
      stats::quantile(
        it.col,
        probs = c(
          0.01, 0.025, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4,
          0.5,
          0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.975, 0.99
        )
      )
    })

  # Calculate the p-values necessary to obtain the desired joint probabilities.
  # For example, if the aler_alpha is 0.05, the user wants to ensure 0.95 confidence that aler_min < .y AND .y < aler_max. The p_value for this joint probability is smaller than the untransformed p_value
  joint_p <- 1 - sqrt(1 - aler_alpha)

  s <- map(1:ncol(s), \(it.col_idx) {

    it.col <- s[, it.col_idx]

    it.col <- c(
      # Retain first half of values
      it.col[1:match('40%', names(it.col))],

      # Create lower confidence bounds just below the midpoint
      aler_lo_lo = if (!is.null(p_dist)) {
        (it.col[['50%']] +
           p_to_random_value(rand_stats[[it.col_idx]], 'aler_min', joint_p[1])) |>
          unname()
      } else {
        NULL
      },
      aler_lo = if (!is.null(p_dist)) {
        (it.col[['50%']] +
           p_to_random_value(rand_stats[[it.col_idx]], 'aler_min', joint_p[2])) |>
          unname()
      } else {
        NULL
      },

      it.col[match('50%', names(it.col))],

      mean = mean(var_vals, na.rm = TRUE),

      # Create upper confidence bounds just above the midpoint
      aler_hi = if (!is.null(p_dist)) {
        (it.col[['50%']] +
           p_to_random_value(rand_stats[[it.col_idx]], 'aler_max', joint_p[2])) |>
          unname()
      } else {
        NULL
      },
      aler_hi_hi = if (!is.null(p_dist)) {
        (it.col[['50%']] +
           p_to_random_value(rand_stats[[it.col_idx]], 'aler_max', joint_p[1])) |>
          unname()
      } else {
        NULL
      },

      # Retain latter half of values
      it.col[match('60%', names(it.col)):length(it.col)]
    )

    # Determine the limits and average of y.
    # min and max are needed only for plotting, but avg is needed for data.
    # Set the plotting boundaries for the y axis
    if (min(var_vals) >= 0 && max(var_vals) <= 1) {  # var is a probability
      it.col <- c(min = 0, it.col)
      it.col <- c(it.col, max = 1)
    }
    else {
      it.col <- c(min = it.col[['1%']], it.col)
      it.col <- c(it.col, max = it.col[['99%']])
    }   # as of now, no treatment and no error for non-numeric y

    it.col
  }) |>
  set_names(colnames(s)) |>
  do.call(cbind, args = _)

  # For categorical variables, create a summary column as the first column
  if (ncol(s) > 1) {
    var_s <- apply(s, 1, median)

    var_s['min']      <- min(s['min', ])
    var_s['mean']     <- mean(s['mean', ])
    var_s['50%']      <- median(s['50%', ])
    var_s['max']      <- max(s['max', ])

    if (!is.null(p_dist)) {
      var_s['aler_lo_lo'] <- min(s['aler_lo_lo', ])
      var_s['aler_lo']   <- min(s['aler_lo', ])
      var_s['aler_hi']   <- max(s['aler_hi', ])
      var_s['aler_hi_hi'] <- max(s['aler_hi_hi', ])
    }

    s <- cbind(
      var_s,
      s
    )
  }

  # The first column should always be named for the var_name, whether it is the only column or not
  colnames(s)[1] <- var_name

  return(s)
}  # var_summary()


# Summarize overlapping confidence regions
summarize_conf_regions_1D <- function(
    ale_data_list,  # list of ale_data elements
    y_summary  # result of var_summary(y_vals)
) {
  # Create zeroed version of y_summary to correspond to zeroed ALE y values.
  # Note: Shifting by the median seems more appropriate than by the mean based on experimenting with the random x4 on the ALEPlot nnet simulation.
  y_zeroed_summary <- y_summary[, 1] - y_summary[['50%', 1]]

  # Create confidence regions for each variable (term)
  cr_by_term <-
    ale_data_list |>
    map(\(it.ale_data) {
      x_name <- names(it.ale_data)[1] |>
        str_remove("\\.bin$|\\.ceil$")


      # cr is the confidence regions for a single variable (term) at a time
      cr <-
        it.ale_data |>
        mutate(
          # where is the current point relative to the ALER band?
          aler_band = case_when(
            .data$.y_hi < y_zeroed_summary['aler_lo'] ~ 'below',
            .data$.y_lo > y_zeroed_summary['aler_hi'] ~ 'above',
            .default = 'overlap'
          ) |>
            factor(ordered = TRUE, levels = c('below', 'overlap', 'above')),
          # new_streak == TRUE if current row has different aler_band from previous row
          new_streak = .data$aler_band != lag(
            .data$aler_band,
            default = first(.data$aler_band)
          ),
          # unique ID for each consecutive streak
          streak_id = cumsum(.data$new_streak)
        )


      # if (var_type(it.ale_data$ale_x) == 'numeric') {
      if (names(cr)[1] |> endsWith('.ceil')) {  # x is numeric
        # Rename the x variable in cr for easier coding
        names(cr)[1] <- '.x'

        cr <- cr |>
          summarize(
            .by = 'streak_id',
            start_x = first(.data$.x),
            end_x = last(.data$.x),
            start_y = first(.data$.y),
            end_y = last(.data$.y),
            n = sum(.data$.n),
            pct = (n / sum(it.ale_data$.n)) * 100,
            aler_band = first(.data$aler_band),
          ) |>
          mutate(
            # diff between start_x and end_x as percentage of the domain of x
            # Convert differences to numeric to handle dates and maybe other unusual types
            x_span_pct = (as.numeric(.data$end_x - .data$start_x) /
              as.numeric(diff(range(it.ale_data[[1]])))) *
              100,
            trend = if_else(
              .data$x_span_pct != 0,
              # slope from (start_x, start_y) to (end_x, end_y) normalized on scales of x and y
              ((.data$end_y - .data$start_y) /
                 (y_zeroed_summary['max'] - y_zeroed_summary['min'])) /
                (.data$x_span_pct / 100),
              0
            )
          ) |>
          select(
            'start_x', 'end_x', 'x_span_pct',
            'start_y', 'end_y', 'trend',
            'aler_band',
            'n', 'pct'
          )

      } else {  # non-numeric x
        # Rename the x variable in cr for easier coding
        names(cr)[1] <- 'x'

        cr <- cr |>
          rename(
            n = '.n',
            y = '.y',
          ) |>
          mutate(
            pct = (.data$n / sum(it.ale_data$.n)) * 100,
            # Convert x column from ordinal to character for consistency across terms
            x = as.character(.data$x),
          ) |>
          select('x', 'y', 'aler_band', 'n', 'pct')
      }

      cr |>
        mutate(
          term = x_name
        )

    }) |>
    set_names(names(ale_data_list)) |>
    bind_rows() |>
    # https://bard.google.com/chat/ea68c7b9e8437179
    select(
      'term',
      # any_of is used because categorical variables do not have 'start_x', 'end_x', 'x_span_pct' while numeric values do not have 'x'
      any_of(c('x', 'start_x', 'end_x', 'x_span_pct')),
      'n', 'pct',
      any_of(c('y', 'start_y', 'end_y', 'trend')),
      'aler_band'
    )

  return(cr_by_term)
}  # summarize_conf_regions_1D()


# Summarize overlapping confidence regions for 2D ALE
summarize_conf_regions_2D <- function(
    ale_data_list,  # list of ale_data elements
    y_summary  # result of var_summary(y_vals)
) {
  # Create terciles of a numeric vector
  terciles <- function(x) {
    tryCatch(
      # cut() crashes if there are duplicate quantiles but is preferred otherise because of its pretty printing
      {
        cut(
          x,
          breaks = quantile(x, probs = c(0, 1/3, 2/3, 1)),
          include.lowest = TRUE,
          # right=TRUE is crucial otherwise dates crash because their cut method has different defaults
          right = TRUE
        )
      },
      # if cut() crashes, fall back on the more robust .bincode() without pretty printing
      error = \(e) {  # nocov start
        .bincode(
          x,
          breaks = quantile(x, probs = c(0, 1/3, 2/3, 1)),
          include.lowest = TRUE
        )
      }  # nocov end
    )
  }


  # Create zeroed version of y_summary to correspond to zeroed ALE y values.
  # Note: Shifting by the median seems more appropriate than by the mean based on experimenting with the random x4 on the ALEPlot nnet simulation.
  y_zeroed_summary <- y_summary[, 1] - y_summary[['50%', 1]]

  # The total n is the total of any of the ALE data .n counts; just pick the first one
  total_n <- sum(ale_data_list[[1]]$.n)

  # Create confidence regions for each variable (term)
  cr_by_term <-
    ale_data_list |>
    map(\(it.ale_data) {
      x1_x2_names <- names(it.ale_data)[1:2]

      # cr is the confidence regions for a single 2D interaction at a time
      cr <-
        it.ale_data |>
        filter(.data$.n != 0) |>
        mutate(
          # where is the current point relative to the ALER band?
          aler_band = case_when(
            .data$.y_hi < y_zeroed_summary['aler_lo'] ~ 'below',
            .data$.y_lo > y_zeroed_summary['aler_hi'] ~ 'above',
            .default = 'overlap'
          ) |>
            factor(ordered = TRUE, levels = c('below', 'overlap', 'above')),
        ) |>
        select(-c('.y_lo':'.y_hi')) |>
        rename(
          n = '.n',
          y = '.y'
        )

      # Group numeric x variables into quantiles of three (terciles), if available
      if ((x1_x2_names[1] |> endsWith('.ceil'))) {
        # Use .bincode() instead of cut() to give evenly spread terciles, even if some tertiles are duplicated. Otherwise, cut() crashes with duplicated tertiles.
        # https://stackoverflow.com/a/26305952/2449926
        cr$x1 <- terciles(cr[[1]])
      }
      # Simply rename non-numeric columns as x1 or x2
      else if ((x1_x2_names[1] |> endsWith('.bin'))) {
        names(cr)[1] <- 'x1'
      }

      # Repeat for x2
      if ((x1_x2_names[2] |> endsWith('.ceil'))) {
        cr$x2 <- terciles(cr[[2]])
      }
      else if ((x1_x2_names[2] |> endsWith('.bin'))) {
        names(cr)[2] <- 'x2'
      }

      cr <- cr |>
        summarize(
          .by = c('x1', 'x2', 'aler_band'),
          n   = sum(.data$n),
          pct = (n / total_n) * 100,
          y   = mean(.data$y),
        ) |>
        # Convert x data columns uniformly to character format
        mutate(
          across(all_of(c('x1', 'x2')), as.character)
        )

      # Rename the x variables with their original variable names
      x1_x2_names <- x1_x2_names |>
        str_remove("\\.bin$|\\.ceil$")

      # Return value for map function
      cr |>
        mutate(
          term1 = x1_x2_names[1],
          term2 = x1_x2_names[2],
        ) |>
        select('term1', 'x1', 'term2', 'x2', everything())
    }) |>
    set_names(names(ale_data_list)) |>
    bind_rows()


  return(cr_by_term)
}  # summarize_conf_regions_1D()

# nocov start
# Receives a confidence region summary tibble and then converts its essential contents in words.
summarize_conf_regions_1D_in_words <- function(
    conf_region_summary,
    start_cap = TRUE  # first character capitalized
) {
  summ <- map_chr(1:nrow(conf_region_summary), \(.row_num) {
    with(
      conf_region_summary[.row_num, ],
      if (exists('start_x')) { # conf_region_summary is numeric
        str_glue(
          'From {round_dp(start_x)} to {round_dp(end_x)}, ',
          'ALE ',
          if_else(
            aler_band == 'overlap',
            'overlaps',
            paste0('is ', aler_band)
          ),
          ' the ALER band ',
          'from {round_dp(start_y)} to {round_dp(end_y)}.'
        )
      } else { # conf_region_summary is NOT numeric
        str_glue(
          'For {x}, the ALE of {round_dp(y)} ',
          if_else(
            aler_band == 'overlap',
            'overlaps',
            paste0('is ', aler_band)
          ),
          ' the ALER band.'
        )
      }
    )
  }) |>
    paste(collapse = ' ')

  if (!start_cap) {
    summ <- tolower(str_sub(summ, 1, 1)) %+% str_sub(summ, 2)
  }

  return(summ)
}  # summarize_conf_regions_in_words()
# nocov end
