#' Plot causal forest tuning results
#'
#' If hyperparameter tuning was performed during the causal forest fit, this
#' function will plot the error as a function of one of the tuned parameters.
#'
#' @param x The \code{tuning_output} object that is part of a
#'   trained \code{\link[grf]{causal_forest}} object
#' @param param The parameter to plot on the horizontal axis. Defaults to the
#'   first parameter in \code{tuning_output$params}
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 plot object
#' @export
#'
#' @family plotting methods
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'  cf <- causal_forest(X, Y, W, tune.parameters = 'min.node.size')
#'
#'  plot(cf$tuning_output)
#' }
plot.tuning_output <- function(x, param = NULL, ...) {
  error <- best <- tuning_output <- NULL
  if (length(x$params) < 1) {
    stop('Error: No tuned parameters found.')
  }
  if (is.null(param)) param <- names(x$params)[1]

  best_param <- x$params[[param]]
  tun <- dplyr::as_tibble(x$grid) %>%
    dplyr::mutate(best = (error == min(error)))

  ggplot2::ggplot(tun, ggplot2::aes(!!sym(param), error, color = best, size = best)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c(`TRUE` = 'black', `FALSE` = 'grey'), guide = F) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 1), guide = F) +
    ggplot2::annotate('text', x = best_param, y = tuning_output$error, label = paste0(param, ': ', best_param),
                      hjust = -0.1, vjust = 1.1) +
    ggplot2::labs(title = 'Causal Forest CV Parameter Tuning Results',
                  subtitle = paste('Parameter to be tuned:', param),
                  x = param,
                  y = 'Error')
}


#' Plot a histogram of estimated CATEs
#'
#' @param results The output from \code{\link{tidy_cf}}
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
#'  cf <- causal_forest(X, Y, W)
#'  results <- tidy_cf(cf)
#'
#'  plot_cate(results)
#' }
plot_cate <- function(results) {
  cate <- NULL

  ggplot2::ggplot(results, ggplot2::aes(cate)) +
    ggplot2::geom_density() +
    ggplot2::labs(title = 'Distribution of Predicted Treatment Effects',
                  x = 'Predicted TE',
                  y = '')
}


#' Plot estimated CATEs versus a covariate
#'
#' @param dat The original training data
#' @param res The output from \code{\link{tidy_cf}}
#' @param covariate The covariate to plot on the horizontal axis
#' @param level The desired confidence level. If \code{NULL}, no confidence
#'   levels are plotted.
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
#'  results <- tidy_cf(cf)
#'  dat <- dplyr::bind_cols(as.data.frame(X), results)
#'  dat$a <- sample(letters[1:3], size = n, replace = T)
#'
#'  plot_predictions_vs_covariate(dat, 'V1', level = 0.9)
#'  plot_predictions_vs_covariate(dat, 'a', level = 0.9)
#' }
plot_predictions_vs_covariate <- function(dat, res, covariate, level = NULL) {
  cate <- cate.se <- NULL
  dat <- dplyr::bind_cols(dat, res)

  if (is.numeric(dat[[covariate]])) {
    p <- dat %>%
      dplyr::arrange(!!sym(covariate)) %>%
      ggplot2::ggplot(ggplot2::aes(!!sym(covariate), cate)) +
      ggplot2::geom_hline(yintercept = 0)
    if (!is.null(level)) {
      z <- 1 - (1-level)/2
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = cate - stats::qnorm(z)*cate.se,
                                          ymax = cate + stats::qnorm(z)*cate.se),
                             fill = 'grey', alpha = 0.6)
    }
    p <- p + ggplot2::geom_point(size = 0.8, alpha = 0.1)
  } else {
    p <- dat %>%
      ggplot2::ggplot(ggplot2::aes(!!sym(covariate), cate)) +
      ggplot2::geom_hline(yintercept = 0, color = 'grey') +
      ggplot2::geom_violin(draw_quantiles = 0.5, adjust = 0.4, scale = 'count',
                           fill = 'dodgerblue')
  }
  p <- p +
    ggplot2::labs(title = 'Predicted Treatment Effects',
                  subtitle = paste('as a function of', covariate),
                  x = covariate,
                  y = 'Predicted TE')
  p
}
