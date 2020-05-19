#' Plot a histogram of fitted propensities
#'
#' The causal forest requires the assumption that we cannot deterministically tell the
#' treatment status of an individual given its covariates. In other words, none of the
#' propensity scores should be near zero or one.
#'
#' @param results A trained causal forest object from \code{\link[grf]{causal_forest}}
#' @param ... Additional arguments to be passed to \code{\link[ggplot2]{geom_histogram}}
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
#'
#'  plot_propensities(cf)
#' }
plot_propensities <- function(results, ...) {
  W.hat <- NULL

  cap <- stringr::str_wrap('The causal forest requires the assumption that we cannot deterministically tell the
                            treatment status of an individual given its covariates. In other words, none of the
                            propensity scores should be near zero or one.',
                           width = 120)

  ggplot2::ggplot(results, ggplot2::aes(W.hat)) +
    ggplot2::geom_histogram(...) +
    ggplot2::labs(title = 'Estimated propensity scores',
                  x = 'Propensity score (W.hat)',
                  y = 'Count',
                  caption = cap)
}


#' @rdname plot_covariate_balance
#'
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{geom_histogram}}
#' @export
plot_covariate_balance_numeric <- function(dat, results, covars = NULL, ...) {
  treatment <- IPW <- value <- NULL

  if (is.null(covars)) {
    covars <- dat %>%
      dplyr::select_if(is.numeric) %>%
      names()
  }
  df <- dat %>%
    dplyr::select(tidyselect::any_of(covars)) %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::bind_cols(dplyr::select(results, treatment, IPW))

  df %>%
    tidyr::pivot_longer(-c(IPW, treatment)) %>%
    ggplot2::ggplot(ggplot2::aes(value, weight = IPW, fill = treatment)) +
    ggplot2::geom_histogram(position = 'identity', alpha = 0.5, ...) +
    ggplot2::facet_wrap(~ name, scales = 'free') +
    ggplot2::labs(title = 'Treatment/Control Balance, Numeric Covariates',
                  subtitle = 'Inverse-propensity weighted',
                  x = '',
                  y = 'Count')
}

#' @rdname plot_covariate_balance
#'
#' @export
plot_covariate_balance_categorical <- function(dat, results, covars = NULL) {
  treatment <- IPW <- NULL

  plotvar <- function(x, df) {
    x <- rlang::sym(x)
    ggplot2::ggplot(df, ggplot2::aes(y = treatment, weight = IPW, fill = !!x)) +
      ggplot2::geom_bar(position = 'fill')
  }

  if (is.null(covars)) {
    covars <- dat %>%
      dplyr::select_if(~ !is.numeric(.)) %>%
      names()
  }

  df <- dat %>%
    dplyr::select(tidyselect::any_of(covars)) %>%
    dplyr::select_if(~ !is.numeric(.)) %>%
    dplyr::bind_cols(dplyr::select(results, treatment, IPW))

  plots <- lapply(covars, plotvar, df)

  if (!requireNamespace('cowplot', quietly = T)) {
    warning('The "cowplot" package must be installed to arrange a grid of
             multiple plots. Returning the list of plots instead.')
    return(plots)
  } else {
    cowplot::plot_grid(plotlist = plots,
                       ncol = min(length(covars), 3),
                       align = 'hv')
  }
}

