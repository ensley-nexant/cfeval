new_ate <- function(a) {
  stopifnot(is.double(a), length(a) == 2, identical(names(a), c('estimate', 'std.err')))
  a <- tibble::as_tibble_row(a)

  structure(
    a,
    class = c('ate', 'tbl_df', 'tbl', 'data.frame')
  )
}

#' Estimate average treatment effects using a causal forest
#'
#' A thin wrapper around \code{\link[grf]{average_treatment_effect}}. Defines
#' the \code{ate} class so that confidence intervals and plots can be created
#' using generics.
#'
#' @param fit A trained \code{\link[grf]{causal_forest}} object
#' @param ... Additional parameters to be passed to
#'   \code{\link[grf]{average_treatment_effect}}
#'
#' @return An instance of the \code{ate} class, which is essentialy a one-row
#'   tibble with columns \code{estimate} and \code{std.err}
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
#'  a <- ate(cf, target.sample = 'treated')
#' }
ate <- function(fit, ...) {
  a <- grf::average_treatment_effect(fit, ...)
  new_ate(a)
}

#' Confidence intervals for causal forest ATEs
#'
#' Computes confidence intervals for the average treatment effect estimated by a
#' \code{\link[grf]{causal_forest}} model.
#'
#' @param object An \code{\link{ate}} object
#' @param parm Not used
#' @param level The desired confidence level
#' @param ... Additional arguments (ignored)
#'
#' @return A one-row tibble with two columns giving lower and upper confidence
#'   bounds for the estimated ATE. The column names will be \eqn{(1-level)/2}%
#'   and \eqn{1 - (1-level)/2}% (by default, `5.0%` and `95.0%`).
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
#'  a <- ate(cf)
#'  confint(a)
#' }
confint.ate <- function(object, parm, level = 0.9, ...) {
  z <- (1-level)/2
  zs <- c(z, 1 - z)
  ci <- object$estimate + object$std.err * stats::qnorm(zs)
  names(ci) <- scales::percent(zs, accuracy = 0.1)
  tibble::as_tibble_row(ci)
}

#' Plot a causal forest ATE
#'
#' Plots the average treatment effect estimated by a
#' \code{\link[grf]{causal_forest}} model.
#'
#' If \code{level = NULL}, the default, no confidence bands will be plotted.
#'
#' @param x An \code{\link{ate}} object
#' @param level The desired confidence level
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
#'
#'  cf <- causal_forest(X, Y, W)
#'  a <- ate(cf)
#'  plot(a)
#' }
plot.ate <- function(x, level = NULL, ...) {
  estimate <- low <- high <- NULL
  subtitle <- NULL

  df <- tibble::as_tibble(x)

  if (!is.null(level)) {
    ci <- confint.ate(x, level = level)
    names(ci) <- c('low', 'high')
    df <- dplyr::bind_cols(df, ci)
    subtitle = paste('With', scales::percent(level, accuracy = 1), 'CI')
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = 'ATE')) +
    ggplot2::geom_point(size = 4)

  if (!is.null(level)) {
    p <- p +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = low, xmax = high),
                              height = 0.2)
  }
  p <- p +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::labs(title = 'Average treatment effect',
                  subtitle = subtitle,
                  x = 'ATE',
                  y = '')

  p
}
