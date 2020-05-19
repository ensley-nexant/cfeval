#' Plot calibration test results
#'
#' Display the results of \code{\link[grf]{test_calibration}} in graphical
#' format.
#'
#' From the \code{grf} documentation:
#'
#' The forest summary function \code{\link[grf]{test_calibration}} can be used
#' to asses a forest's goodness of fit. A coefficient of 1 for
#' \code{mean.forest.prediction} suggests that the mean forest prediction is
#' correct and a coefficient of 1 for \code{differential.forest.prediction}
#' suggests that the forest has captured heterogeneity in the underlying signal.
#'
#' @param clbr The output of \code{\link[grf]{test_calibration}}
#' @param ci The size of the confidence intervals. Default is 90%
#'
#' @return A ggplot2 plot object
#' @export
#'
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'  cf <- causal_forest(X, Y, W)
#'
#'  clbr <- test_calibration(cf)
#'  plot_calibration_test(clbr, ci = 0.95)
#' }
plot_calibration_test <- function(clbr, ci = 0.9) {
  estimate <- std.error <- term <- p.value <- NULL
  cap <- stringr::str_wrap('A coefficient of 1 for mean.forest.prediction suggests that the mean forest prediction is accurate.
                            The closer the coefficient for differential.forest.prediction is to 1, the more
                            heterogeneity was captured by the causal forest. The p value for differential.forest.prediction
                            can also serve as a test for heterogeneity.',
                           width = 120)
  z <- get_ci_const(ci)

  broom::tidy(clbr) %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate,
                                 xmin = estimate - stats::qnorm(z)*std.error,
                                 xmax = estimate + stats::qnorm(z)*std.error,
                                 y = term,
                                 label = paste('p =', format(p.value, digits = 3)))) +
    ggplot2::geom_vline(xintercept = 1, color = 'grey') +
    ggplot2::geom_col(width = 0.5, fill = 'dodgerblue') +
    ggplot2::geom_pointrange(size = 1) +
    ggplot2::geom_text(ggplot2::aes(x = 0), hjust = -0.1) +
    ggplot2::labs(title = 'Causal Forest Calibration Test',
                  x = 'Estimate',
                  y = 'Term',
                  caption = cap)
}



#' Plot a histogram of estimated bias
#'
#' @param results The output from \code{\link{tidy_cf}}
#' @param ... Additional arguments passed to \code{\link[ggplot2]{geom_histogram}}.
#'
#' @return A ggplot2 plot object
#'
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'
#'  # fit the model and extract results
#'  cf <- causal_forest(X, Y, W)
#'  results <- tidy_cf(cf)
#'
#'  plot_bias(results, bins = 30)
#' }
plot_bias <- function(results, ...) {
  bias <- Y <- NULL

  ggplot2::ggplot(results, ggplot2::aes(bias / stats::sd(Y))) +
    ggplot2::geom_histogram(...) +
    ggplot2::labs(title = 'Bias distribution',
                  x = 'Bias (in standard deviations)',
                  y = 'Count')
}


#' Find difference in ATE between subgroups
#'
#' One heuristic for evaluating whether a causal forest has detected
#' heterogeneity is to group observations into high- and low-CATE subgroups,
#' estimate the ATE for each group, and determine whether the difference is
#' significantly different than zero.
#'
#' @param fit A trained causal forest object from
#'   \code{\link[grf]{causal_forest}}
#' @param subgroup A logical vector indicating whether an observation belongs to
#'   the subgrouping
#' @param level The width of the reported confidence interval
#'
#' @return A one-row tibble with the following columns: \describe{
#'   \item{\code{in_est}, \code{in_se}}{The ATE estimate and corresponding
#'   standard error among observations for which \code{subgroup = TRUE}}
#'   \item{\code{out_est}, \code{out_se}}{The ATE estimate and corresponding
#'   standard error among observations for which \code{subgroup = FALSE}}
#'   \item{\code{diff_est}}{The difference in ATE: \code{in_est} -
#'   \code{out_est}} \item{\code{diff_ci_low}, \code{diff_ci_high}}{The
#'   confidence interval around \code{diff_est}}}
#' @export
#'
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'
#'  cf <- causal_forest(X, Y, W)
#'  results <- tidy_cf(cf)
#'
#'  # partition into high and low CATE subgroups
#'  estimate_subgroup_ate(cf, results$cate > median(results$cate))
#' }
estimate_subgroup_ate <- function(fit, subgroup, level = 0.9) {
  z <- (1 - level) / 2
  zs <- c(z, 1 - z)

  in_ate <- ate(fit, subset = subgroup)
  out_ate <- ate(fit, subset = !subgroup)
  diff_est <- in_ate$estimate - out_ate$estimate
  diff_se <- sqrt(in_ate$std.err^2 + out_ate$std.err^2)
  diff_ci <- diff_est + stats::qnorm(zs) * diff_se

  res <- c(in_ate$estimate,
           out_ate$estimate,
           in_ate$std.err,
           out_ate$std.err,
           diff_est,
           diff_ci)
  names(res) <- c('in_est', 'out_est', 'in_se', 'out_se',
                  'diff_est', 'diff_ci_low', 'diff_ci_high')

  tibble::as_tibble_row(res)
}

#' Plot ATE differences by one or more subgroups
#'
#' Visualize the heterogeneity heuristic described in
#' \code{\link{estimate_subgroup_ate}}. If more than one subgroup is given, they
#' are all displayed in the plot.
#'
#' If \code{subgroups} is a \code{data.frame}, the columns are assumed to be the
#' subgroups. They should all be logical, otherwise unexpected behavior may
#' occur.
#'
#' @param fit A trained causal forest object from
#'   \code{\link[grf]{causal_forest}}
#' @param subgroups A logical vector indicating whether an observation belongs
#'   to the subgrouping, or a list/data frame of such vectors.
#' @param level The width of the reported confidence intervals
#'
#' @return A ggplot2 plot object
#' @export
#'
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'
#'  cf <- causal_forest(X, Y, W)
#'  results <- tidy_cf(cf)
#'
#'  grps <- data.frame(
#'    high_cate = results$cate > median(results$cate),
#'    high_X3 = X[, 3] > median(X[, 3])
#'  )
#'  plot_subgroup_ates(cf, grps)
#' }
plot_subgroup_ates <- function(fit, subgroups, level = 0.9) {
  subgroup <- diff_est <- diff_ci_low <- diff_ci_high <- NULL

  if (is.vector(subgroups)) subgroups <- list(GRP = subgroups)
  ate_diffs <- purrr::map_dfr(subgroups, ~ estimate_subgroup_ate(fit, ., level = level), .id = 'subgroup')
  ate_diffs <- ate_diffs %>%
    dplyr::mutate(subgroup = forcats::fct_relevel(subgroup, !!!rev(names(subgroups))))

  cap <- stringr::str_wrap('A bar further to the right indicates that the ATE among observations
                            belonging to the subgroup is greater than the ATE among observations
                            not belonging to the subgroup.',
                           width = 120)
  ggplot2::ggplot(ate_diffs, ggplot2::aes(x = diff_est, xmin = diff_ci_low, xmax = diff_ci_high, y = subgroup)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::geom_pointrange() +
    ggplot2::labs(title = 'Difference in ATE by subgroups',
                  x = 'ATE Difference',
                  y = 'Subgroup',
                  caption = cap)
}

