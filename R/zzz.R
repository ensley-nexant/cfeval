#' Check covariate balance
#'
#' After accounting for propensity, covariate distributions should be balanced
#' between treated and control observations. These functions plot overlaid
#' histograms, one for treated and one for control, of each covariate, using
#' inverse propensity weights.
#'
#' @name plot_covariate_balance
#' @aliases plot_covariate_balance_numeric plot_covariate_balance_categorical
#'
#' @param dat A data frame of covariates
#' @param results The output from \code{\link{tidy_cf}}
#' @param covars A character vector of covariates to include in the plot. If
#'   omitted, all columns of \code{dat} for which \code{is.numeric = TRUE} (for
#'   the numeric version) or \code{is.numeric = FALSE} (for the categorical
#'   version) will be plotted.
#'
#' @return A ggplot2 plot object, or list of plot objects if \code{plot =
#'   FALSE}.
#'
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  dat <- as.data.frame(X)
#'  dat$a <- sample(letters[1:3], size = n, replace = T)
#'  dat$b <- sample(letters[10:17], size = n, replace = T)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'
#'  # fit the model and extract results
#'  cf <- causal_forest(X, Y, W)
#'  results <- tidy_cf(cf)
#'
#'  plot_covariate_balance_numeric(dat, results, bins = 20)
#'  plot_covariate_balance_categorical(dat, results, plot = T)
#' }
NULL
