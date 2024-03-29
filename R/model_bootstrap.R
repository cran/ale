##' model_bootstrap.R
#'

#' Execute full model bootstrapping with ALE calculation on each bootstrap run
#'
#' No modelling results, with or without ALE, should be considered reliable without
#' being bootstrapped. For large datasets, normally the model provided to [ale()]
#' is the final deployment model that has been validated and evaluated on
#' training and testing on subsets; that is why [ale()] is calculated on the full
#' dataset. However, when a dataset is too small to be subdivided into training
#' and test sets for a standard machine learning process, then the entire model
#' should be bootstrapped. That is, multiple models should be trained, one on
#' each bootstrap sample. The reliable results are the average results of all
#' the bootstrap models, however many there are. For details, see the vignette
#' on small datasets or the details and examples below.
#'
#' [model_bootstrap()] automatically carries out full-model bootstrapping suitable
#' for small datasets. Specifically, it:
#'
#' * Creates multiple bootstrap samples (default 100; the user can specify any number);
#' * Creates a model on each bootstrap sample;
#' * Calculates model overall statistics, variable coefficients, and ALE values
#'  for each model on each bootstrap sample;
#' * Calculates the mean, median, and lower and upper confidence intervals for
#'  each of those values across all bootstrap samples.
#'
#'  **P-values**
#'  The [broom::tidy()] summary statistics will provide p-values as normal, but the
#'  situation is somewhat complicated with p-values for ALE statistics. The challenge
#'  is that the procedure for obtaining their p-values is very slow: it involves
#'  retraining the model 1000 times. Thus, it is not efficient to calculate p-values
#'  on every execution of `model_bootstrap()`. Although the [ale()] function provides
#'  an 'auto' option for creating p-values,
#'  that option is disabled in `model_bootstrap()` because it would be far too slow:
#'  it would involve retraining the model 1000 times the number of bootstrap iterations.
#'  Rather, you must first create a p-values function object using the procedure
#'  described in `help(create_p_funs)`. If the name of your p-values object is
#'  `p_funs`, you can then request p-values each time you run `model_bootstrap()`
#'  by passing it the argument `ale_options = list(p_values = p_funs)`.
#'
#' @export
#'
#' @references Okoli, Chitu. 2023.
#' “Statistical Inference Using Machine Learning and Classical Techniques Based
#' on Accumulated Local Effects (ALE).” arXiv. <https://arxiv.org/abs/2310.09877>.
#'
#'
#' @param data dataframe. Dataset that will be bootstrapped.
#' @param model See documentation for [ale()]
#' @param ... not used. Inserted to require explicit naming of subsequent arguments.
#' @param model_call_string character string. If NULL, [model_bootstrap()] tries to
#' automatically detect and construct the call for bootstrapped datasets. If it cannot, the
#' function will fail early. In that case, a character string of the full call
#' for the model must be provided that includes `boot_data` as the data argument for the call.
#' See examples.
# Future arguments:
# * y_col: name of y column in data. This would allow SD and MAD to be calculated.
# * pred_fun,pred_type: allows the prediction function to be called; this would
# allow bootstrapped RMSE, MAE, cross entropy, and AUC to be calculated.
#' @param model_call_string_vars character. Character vector of names of variables
#' included in `model_call_string` that are not columns in `data`.
#' If any such variables exist, they must be specified here or else parallel processing
#' will produce an error. If parallelization is disabled with `parallel = 0`,
#' then this is not a concern.
#' @param parallel See documentation for [ale()]
#' @param model_packages See documentation for [ale()]
#' @param boot_it integer from 0 to Inf. Number of bootstrap iterations.
#' If boot_it = 0, then the model is run as normal once on the full `data` with
#' no bootstrapping.
#' @param seed integer. Random seed. Supply this between runs to assure identical
#' bootstrap samples are generated each time on the same data.
#' @param boot_alpha numeric. The confidence level for the bootstrap confidence intervals is
#' 1 - boot_alpha. For example, the default 0.05 will give a 95% confidence
#' interval, that is, from the 2.5% to the 97.5% percentile.
#' @param boot_centre See See documentation for [ale()]
#' @param output character vector. Which types of bootstraps to calculate and return:
#' * 'ale': Calculate and return bootstrapped ALE data and plot.
#' * 'model_stats': Calculate and return bootstrapped overall model statistics.
#' * 'model_coefs': Calculate and return bootstrapped model coefficients.
#' * 'boot_data': Return full data for all bootstrap iterations. This data will always be calculated
#' because it is needed for the bootstrap averages. By default, it is not returned
#' except if included in this `output` argument.
#' @param ale_options,tidy_options,glance_options list of named arguments.
#' Arguments to pass to the [ale()], [broom::tidy()], or [broom::glance()] functions, respectively,
#' beyond (or overriding) the defaults. In particular, to obtain p-values for ALE
#' statistics, see the details.
#' @param compact_plots See documentation for [ale()]
#' @param silent See documentation for [ale()]
#'
#' @return list with tibbles of the following elements (depending on values requested in
#' the `output` argument:
#' * model_stats: bootstrapped results from [broom::glance()]
#' * model_coefs: bootstrapped results from [broom::tidy()]
#' * ale: bootstrapped ALE results
#'   * data: ALE data (see [ale()] for details about the format)
#'   * stats: ALE statistics. The same data is duplicated with different views
#'   that might be variously useful. The column
#'     * by_term: statistic, estimate, conf.low, median, mean, conf.high.
#'     ("term" means variable name.)
#'     The column names are compatible with the `broom` package. The confidence intervals
#'     are based on the [ale()] function defaults; they can be changed with the
#'     `ale_options` argument. The estimate is the median or the mean, depending
#'     on the `boot_centre` argument.
#'     * by_statistic: term, estimate, conf.low, median, mean, conf.high.
#'     * estimate: term, then one column per statistic Provided with the default
#'     estimate. This view does not present confidence intervals.
#'   * plots: ALE plots (see [ale()] for details about the format)
#' * boot_data: full bootstrap data (not returned by default)
#' * other values: the `boot_it`, `seed`, `boot_alpha`, and `boot_centre` arguments that
#' were originally passed are returned for reference.
#'
#' @examples
#'
#' # attitude dataset
#' attitude
#'
#' ## ALE for general additive models (GAM)
#' ## GAM is tweaked to work on the small dataset.
#' gam_attitude <- mgcv::gam(rating ~ complaints + privileges + s(learning) +
#'                             raises + s(critical) + advance,
#'                           data = attitude)
#' summary(gam_attitude)
#'
#' \donttest{
#' # Full model bootstrapping
#' # Only 4 bootstrap iterations for a rapid example; default is 100
#' # Increase value of boot_it for more realistic results
#' mb_gam <- model_bootstrap(
#'   attitude,
#'   gam_attitude,
#'   boot_it = 4,
#'   parallel = 2  # CRAN limit (delete this line on your own computer)
#' )
#'
#' # If the model is not standard, supply model_call_string with
#' # 'data = boot_data' in the string (not as a direct argument to [model_bootstrap()])
#' mb_gam <- model_bootstrap(
#'   attitude,
#'   gam_attitude,
#'   model_call_string = 'mgcv::gam(
#'     rating ~ complaints + privileges + s(learning) +
#'       raises + s(critical) + advance,
#'     data = boot_data
#'   )',
#'   boot_it = 4,
#'   parallel = 2  # CRAN limit (delete this line on your own computer)
#' )
#'
#' # Model statistics and coefficients
#' mb_gam$model_stats
#' mb_gam$model_coefs
#'
#' # Plot ALE
#' mb_gam$ale$plots |>
#'   patchwork::wrap_plots()
#' }
#'
#'
#'
#'
#' @import dplyr
#' @import purrr
#' @import assertthat
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom stats sd
#'
model_bootstrap <- function (
    data,
    model,
    ...,
    model_call_string = NULL,
    model_call_string_vars = character(),
    parallel = parallel::detectCores(logical = FALSE) - 1,
    model_packages = as.character(NA),
    # y_col,
    # pred_fun,
    # pred_type,
    boot_it = 100,
    seed = 0,
    boot_alpha = 0.05,
    boot_centre = 'mean',
    output = c('ale', 'model_stats', 'model_coefs'),
    ale_options = list(),
    tidy_options = list(),
    glance_options = list(),
    compact_plots = FALSE,
    silent = FALSE
) {
  # Validate arguments
  ellipsis::check_dots_empty()  # error if any unlisted argument is used (captured in ...)

  assert_that(data |> inherits('data.frame'))

  # If model_call_string is not provided, ensure that
  # the model allows automatic manipulation.
  if (is.null(model_call_string)) {

    # Automatically extract the call from the model
    model_call <- insight::get_call(model)

    assert_that(
      !is.character(model),
      # If there is no model_call_string and model is a character,
      # then model was probably omitted and model_call_string might was
      # mistakenly passed in the model argument position
      msg = '"model" is a required argument.'
    )

    assert_that(
      !is.null(model_call),
      msg = glue::glue(
        'The model call could not be automatically detected, so ',
        'model_call_string must be provided. See help(model_bootstrap) ',
        'for details.'
      )
    )
  }
  else {  # validate model_call_string
    assert_that(is.string(model_call_string))
    assert_that(
      stringr::str_detect(model_call_string, 'boot_data'),
      msg = glue::glue(
        'The data argument for model_call_string must be "boot_data". ',
        'See help(model_bootstrap) for details.'
      )
    )
  }

  model_packages <- validated_parallel_packages(parallel, model, model_packages)

  assert_that(is.whole(boot_it))
  assert_that(is.number(seed))
  assert_that(is.number(boot_alpha) && between(boot_alpha, 0, 1))
  assert_that(boot_centre == 'mean' || boot_centre == 'median')
  # output must be a subset of c('ale', 'model_stats', 'model_coefs')
  assert_that(
    length(setdiff(output, c('ale', 'model_stats', 'model_coefs'))) == 0,
    msg = 'The value in the output argument must be one or more of
    "ale", "model_stats", or "model_coefs".'
  )

  assert_that(is.list(ale_options))
  assert_that(
    !(
      !is.null(ale_options$p_values) &&
        length(ale_options$p_values) == 1 &&
        ale_options$p_values == 'auto'
    ),
    msg = paste0(
      'The `ale_options` `p_values == "auto"` option is disabled for `model_bootstrap()` ',
      'because it is far too slow. Rather, you must pass a p-values ',
      'function object using the procedure described in `help(create_p_funs)`.'
    )
  )
  assert_that(is.list(tidy_options))
  assert_that(is.list(glance_options))

  validate_silent(silent)


  n_rows <- nrow(data)



  # Create bootstrap tbl
  set.seed(seed)
  boot_data <- tibble(
    # it: bootstrap iteration number.
    # Row 0 is the full dataset without bootstrapping
    it = 0:boot_it,
    # row_idxs: row indexes of each bootstrap sample.
    # Store just the indexes rather than duplicating the entire dataset
    #   multiple times.
    row_idxs = map(0:boot_it, \(.it) {
      if (.it == 0) {  # row 0 is the full dataset without bootstrapping
        1:n_rows
      } else {  # bootstrap: sample n_rows with replacement
        sample.int(n_rows, replace = TRUE)
      }
    })
    )

  # Initialize common ale_x for all iterations
  ale_xs <- NULL
  ale_ns <- NULL

  # Enable parallel processing and set appropriate map function.
  # Because furrr::future_map2 has an important .options argument absent from
  # purrr::map2, map2_loop() is created to unify these two functions.
  if (parallel > 0) {
    future::plan(future::multisession, workers = parallel)
    map2_loop <- furrr::future_map2
  } else {
    # If no parallel processing, do not set future::plan(future::sequential):
    # this might interfere with other parallel processing in a larger workflow.
    # Just do nothing parallel.
    map2_loop <- function(..., .options = NULL) {
      # Ignore the .options argument and pass on everything else
      purrr::map2(...)
    }
  }

  model_call_string_vars <- c(
    'boot_data',
    model_call_string_vars
  )

  # Create progress bar iterator
  if (!silent) {
    progress_iterator <- progressr::progressor(
      # progressor will run once for the full dataset + boot_it times
      steps = boot_it + 1,
      message = 'Creating and analyzing models'
    )
  }

  model_and_ale <-
    map2_loop(
      .options = furrr::furrr_options(
        # Enable parallel-processing random seed generation
        seed = seed,
        # transmit any globals and packages in model_call_string to the parallel workers
        globals = model_call_string_vars,
        packages = model_packages
      ),
      .x = boot_data$it,
      .y = boot_data$row_idxs,
      .f = \(.it, .idxs) {
        # Increment progress bar iterator
        # Do not skip iterations (e.g., .it %% 10 == 0): inaccurate with parallelization
        if (!silent) {
          progress_iterator()
        }

        # boot_data: this particular bootstrap sample
        boot_data <- data[.idxs, ]

        # If model_call_string was provided, prefer it to automatic detection
        if (!is.null(model_call_string)) {
          boot_model <-  # model generated on this particular bootstrap sample
            model_call_string |>
            parse(text = _) |>  # convert model call string to an expression
            eval()
        }
        else {  # use the automatically detected model call
          # Update the model to call to train on boot_data
          model_call$data <- boot_data

          boot_model <- eval(model_call)
        }

        boot_glance <-
          if ('model_stats' %in% output) {
            bg <- do.call(broom::glance, list(boot_model,
                                        unlist(glance_options)))
            # # If we eventually add a predict function, add RMSE and MAE
            # bg$mae <- mae(actual, predicted)
            # bg$rmse <- rmse(actual, predicted)

            bg
          } else {
            NA
          }

        boot_tidy <-
          if ('model_coefs' %in% output) {
            # Unless the user manually specified conf.int, set it to FALSE
            # because this function creates its own bootstrapped confidence intervals.
            if (is.null(tidy_options$conf.int)) {
              tidy_options$conf.int <- FALSE
            }

            do.call(broom::tidy,
                    c(list(boot_model),  # model object
                      tidy_options))  # any parameters
          } else {
            NA
          }


        if ('ale' %in% output) {
          boot_ale <-if (is.na(sum(boot_model$coefficients, na.rm = FALSE))) {
            # One or more coefficients are not defined.
            # This might be due to collinearity in a bootstrapped sample, which
            # yields the warning: "Coefficients: (_ not defined because of singularities)".
            NA
          } else {  # Valid model and ALE requested

            # Calculate ALE. Use do.call so that ale_options can be passed.
            do.call(
              ale_core,
              utils::modifyList(
                list(
                  data = boot_data,
                  model = boot_model,
                  ixn = FALSE,
                  parallel = 0,  # do not parallelize at this inner level
                  boot_it = 0,  # do not bootstrap at this inner level
                  # do not generate plots or request conf_regions
                  output = c('data', 'stats'),
                  ale_xs = if (.it == 0) {
                    NULL
                  } else {
                    ale_xs
                  },
                  ale_ns = if (.it == 0) {
                    NULL
                  } else {
                    ale_ns
                  },
                  silent = TRUE  # silence inner bootstrap loop
                ),
                # pass all other desired options, e.g., specific x_col
                ale_options
              ),
              # assure appropriate scoping with do.call()
              envir = parent.frame(1)
            )
          }

          # From full dataset (.it == 0), calculate common ale_x for all subsequent iterations
          if (.it == 0) {
            # Super-assignment needed to set ale_xs and ale_ns for all iterations,
            # not just the current one
            ale_xs <<-
              boot_ale$data |>
              map(\(.x) .x$ale_x)

            ale_ns <<-
              boot_ale$data |>
              map(\(.x) .x$ale_n)
          }

        }  # end:  if ('ale' %in% output)

        else {  # 'ale' not requested in output
          boot_ale <- NA
        }


        list(
          model = boot_model,
          ale = boot_ale,
          tidy = boot_tidy,
          glance = do.call(broom::glance, list(boot_model,
                                             unlist(glance_options)))
        )

      }
    ) |>
    transpose()

  # Disable parallel processing if it had been enabled
  if (parallel > 0) {
    future::plan(future::sequential)
  }


  # Bind the model and ALE data to the bootstrap tbl
  boot_data <- boot_data |>
    mutate(
      model = model_and_ale$model,
      ale = model_and_ale$ale,
      tidy = model_and_ale$tidy,
      glance = model_and_ale$glance
    )


  ## Summarize the bootstrapped data


  # Bootstrapped model statistics
  glance_summary <-
    if ('model_stats' %in% output) {
      # Model statistics for which bootstrapping is not meaningful.
      # see https://stats.stackexchange.com/a/529506/81392
      invalid_boot_model_stats <- c('logLik', 'AIC', 'BIC', 'deviance')

      boot_data |>
        # filter(it != 0) |>
        # only summarize rows other than the full dataset analysis (it == 0)
        filter(.data$it != if_else(
          boot_it != 0,
          0,  # if boot_it != 0, remove it == 0
          -1  # else, remove nothing; analyze the unique row (it is never -1)
        )) |>
        (`[[`)('glance') |>
        bind_rows() |>
        select(-any_of(invalid_boot_model_stats)) |>
        tidyr::pivot_longer(everything()) |>
        select('name', 'value') |>
        summarize(
          .by = 'name',
          conf.low = quantile(.data$value, boot_alpha / 2, na.rm = TRUE),
          mean = mean(.data$value, na.rm = TRUE),
          median = median(.data$value, na.rm = TRUE),
          conf.high = quantile(.data$value, 1 - (boot_alpha / 2), na.rm = TRUE),
          sd = sd(.data$value, na.rm = TRUE),
          estimate = if_else(boot_centre == 'mean', .data$mean, .data$median)
        ) |>
        select('name', 'estimate', everything())
      # # If y_vals is ever added...
      # |>
      #   bind_rows(tibble(
      #     name = c('sd', 'mad'),
      #     conf.low = c(sd(y_vals), mad(y_vals)),
      #     mean = conf.low,
      #     median = conf.low,
      #     conf.high = conf.low,
      #     sd = 0,
      #   ))
    } else {
      NULL
    }

  # Bootstrapped model coefficient estimates
  tidy_summary <-
    if ('model_coefs' %in% output) {

      # Rename some tidy outputs that do not normally report `estimate`
      tidy_boot_data <-
        boot_data |>
        # filter(it != 0) |>
        # only summarize rows other than the full dataset analysis (it == 0)
        filter(.data$it != if_else(
          boot_it != 0,
          0,  # if boot_it != 0, remove it == 0
          -1  # else, remove nothing; analyze the unique row (it is never -1)
        )) |>
        (`[[`)('tidy') |>
        bind_rows()

      tidy_boot_data_names <- names(tidy_boot_data)
      if (!('estimate' %in% tidy_boot_data_names)) {
        # Explicitly rename some known columns that `tidy` sometimes uses
        # instead of 'estimate'
        if ('edf' %in% tidy_boot_data_names) {  # tidy.gam when parametric = FALSE
          tidy_boot_data$estimate <- tidy_boot_data$edf
        }
      } else if ('edf' %in% tidy_boot_data_names) {  # tidy.gam when parametric = NULL
        tidy_boot_data$estimate <- if_else(
          is.na(tidy_boot_data$estimate),
          tidy_boot_data$edf,
          tidy_boot_data$estimate
        )
      }

      # assign result for tidy_summary
      tidy_boot_data |>
        select('term', 'estimate') |>
        summarize(
          .by = 'term',
          conf.low = quantile(.data$estimate, boot_alpha / 2, na.rm = TRUE),
          mean = mean(.data$estimate, na.rm = TRUE),
          median = median(.data$estimate, na.rm = TRUE),
          conf.high = quantile(.data$estimate, 1 - (boot_alpha / 2), na.rm = TRUE),
          std.error = sd(.data$estimate, na.rm = TRUE),
          estimate = if_else(boot_centre == 'mean', .data$mean, .data$median)
        ) |>
        select('term', 'estimate', everything())
    } else {
      NULL
    }

  # Bootstrapped ALE data with plot
  ale_summary <-
    if ('ale' %in% output) {
      full_ale <- boot_data$ale[[1]]

      # Extract useful details from full model ALE; will be used for plotting
      y_col <- full_ale$y_col
      y_type <- full_ale$y_type
      y_summary <- full_ale$y_summary

      # Remove first element (not bootstrapped) if bootstrapping is requested
      boot_data_ale <-
        if (boot_it == 0) {  # only one full iteration; it is valid
          boot_data$ale
        } else {  # for regular bootstraps, delete the first full model ALE
          boot_data$ale[-1]
        }

      # Summarize bootstrapped ALE data, grouped by variable
      ale_summary_data <-
        boot_data_ale |>
        map(\(.it) .it$data) |>   # extract data from each iteration
        transpose()  # rearrange list to group all iterations by x_col
      ale_summary_data <-
        map2(
          ale_summary_data, names(ale_summary_data),
          \(.x_col, .x_col_name) {

            # If ale_x for .x_col is ordinal,
            # harmonize the levels across bootstrap iterations,
            # otherwise binding rows will fail
            if (is.ordered(.x_col[[1]]$ale_x)) {
              # The levels of the full data ALE are canonical for all bootstrap iterations
              ale_x_levels <- full_ale$data[[.x_col_name]]$ale_x

              .x_col <- .x_col |>
                map(\(.ale_tbl) {
                  .ale_tbl |>
                    mutate(ale_x = ordered(.data$ale_x, levels = ale_x_levels))
                })
            }

            .x_col |>
              bind_rows() |>
              group_by(.data$ale_x) |>
              summarize(
                ale_y_lo = quantile(.data$ale_y, probs = (boot_alpha / 2), na.rm = TRUE),
                ale_y_mean = mean(.data$ale_y, na.rm = TRUE),
                ale_y_median = median(.data$ale_y, na.rm = TRUE),
                ale_y_hi = quantile(.data$ale_y, probs = 1 - (boot_alpha / 2), na.rm = TRUE),
                ale_y = if_else(boot_centre == 'mean', .data$ale_y_mean, .data$ale_y_median),
              ) |>
              right_join(
                tibble(
                  ale_x = full_ale$data[[.x_col_name]]$ale_x,
                  ale_n = full_ale$data[[.x_col_name]]$ale_n,
                ),
                by = 'ale_x'
              ) |>
              select('ale_x', 'ale_n', 'ale_y', everything())
        })

      # Summarize bootstrapped ALE statistics
      ale_summary_stats <-
        boot_data_ale |>
        map(\(.it) .it$stats) |>   # extract stats from each iteration
        transpose()  # rearrange list to group all iterations by x_col (term)

      ale_summary_stats <-
        ale_summary_stats$estimate |>
        bind_rows() |>
        tidyr::pivot_longer(
          cols = 'aled':'naler_max',
          names_to = 'statistic',
          values_to = 'estimate'
        ) |>
        summarize(
          .by = c('term', 'statistic'),
          conf.low = quantile(.data$estimate, probs = (boot_alpha / 2), na.rm = TRUE),
          median = median(.data$estimate, na.rm = TRUE),
          mean = mean(.data$estimate, na.rm = TRUE),
          conf.high = quantile(.data$estimate, probs = 1 - (boot_alpha / 2), na.rm = TRUE),
          estimate = if_else(boot_centre == 'mean', .data$mean, .data$median),
        ) |>
        select('term', 'statistic', 'estimate', everything())

      # If an ALE p-values object was passed, calculate p-values
      if (names(y_summary)[1] == 'p') {
        ale_summary_stats <- ale_summary_stats |>
          rowwise() |>  # required to get statistic function for each row
          mutate(
            p.value = ale_options$p_values$value_to_p[[.data$statistic]](.data$estimate),
          ) |>
          ungroup() |>  # undo rowwise()
          select('term', 'statistic', 'estimate', 'p.value', everything())
      }

      ale_conf_regions <- summarize_conf_regions(
        ale_summary_data,
        y_summary,
        sig_criterion = if (!is.null(ale_options$p_values)) {
          'p_values'
        } else {
          'median_band_pct'
        }
      )

      detailed_ale_stats <- pivot_stats(ale_summary_stats)

      ale_summary_plots <- NULL
      # By default, produce ALE plots except if the user explicitly excluded them
      if (!('output' %in% names(ale_options)) ||  # user didn't specify precise ALE output options
          ('plot' %in% ale_options$output)) {    # or if they did, they at least requested plots
        # Produce ALE plots for each variable
        ale_summary_plots <- map2(
          ale_summary_data, names(ale_summary_data),
          \(.x_col_data, .x_col_name) {
            plot_ale(
              .x_col_data, .x_col_name, y_col, y_type, y_summary,
              # Temporarily buggy for binary y
              x_y = tibble(data[[.x_col_name]], data[[y_col]]) |>
                stats::setNames(c(.x_col_name, y_col)),

              ## Later: pass ale_options() that might apply
              compact_plots = compact_plots

              # When y_vals is added
              # x_y = tibble(data[[.x_col_name]], y_vals) |>
              #   stats::setNames(c(.x_col_name, y_col)),
            )
          }
        )

        # Also produce an ALE effects plot

        # Retrieve median_band_pct if provided; otherwise use boot_alpha
        median_band_pct <- if (is.null(ale_options$median_band_pct)) {
          c(boot_alpha, boot_alpha)
        } else {
          ale_options$median_band_pct
        }

        detailed_ale_stats$effects_plot <- plot_effects(
          detailed_ale_stats$estimate,
          data[[y_col]],
          y_col,
          y_summary,
          # later pass ale_options like compact_plots
          compact_plots = compact_plots
        )

      }

      # Return ALE results
      list(
        data = ale_summary_data,
        stats = detailed_ale_stats,
        plots = ale_summary_plots,
        conf_regions = ale_conf_regions
      )
    }
  # ALE not requested
  else {
      NULL
    }


  return(list(
    model_stats = glance_summary,
    model_coefs = tidy_summary,
    ale = ale_summary,
    boot_data = if ('boot_data' %in% output) {
      boot_data
    } else {
      NULL
    },
    boot_it = boot_it,
    seed = seed,
    boot_alpha = boot_alpha,
    boot_centre = boot_centre
    ))

}


