compat_repair <- function(names, n) {
  if (is.null(names) || any(bad_names <- duplicated(names) | names == '')) {
    compat_names <- paste0('V', seq_len(n))
    if (is.null(names)) {
      names <- compat_names
    } else {
      names[bad_names] <- compat_names[bad_names]
    }
  }
  names
}


new_cf_eval <- function(dat, res, varimp) {
  tibblize <- function(x) tibble::as_tibble(x, .name_repair = ~ compat_repair(., ncol(x)))
  structure(
    list(dat = tibblize(dat),
         res = res,
         varimp = varimp),
    class = 'cf_eval'
  )
}


validate_cf_eval <- function(x) {
  dat <- x$dat
  res <- x$res
  varimp <- x$varimp

  if (!inherits(res, 'results')) {
    stop(
      "'res' must be a 'results' object",
      call. = F
    )
  }

  if (!inherits(varimp, 'varimp')) {
    stop(
      "'varimp' must be a 'varimp' object",
      call. = F
    )
  }

  if (nrow(dat) != nrow(res)) {
    stop(
      "'Xdat' must have the same number of observations as was used to fit the causal forest.",
      call. = F
    )
  }

  x
}



#' Create a causal forest evaluation object
#'
#' @param fit A trained \code{\link[grf]{causal_forest}} object
#' @param Xdat The data frame \code{fit} was trained on. It may include
#'   categorical covariates.
#' @param preds Output from \code{\link[grf]{predict.causal_forest}}. If
#'   omitted, it will be called directly to generate OOB predictions and
#'   corresponding variance estimates. This may be time consuming.
#'
#' @return A \code{cf_eval} object, which is essentially just a list with the
#'   following elements:
#'   \describe{
#'   \item{\code{Xdat}}{The same as the input \code{Xdat} but converted to a \link[tibble:tibble-package]{tibble}}
#'   \item{\code{res}}{A \link{results} object}
#'   \item{\code{varimp}}{A \link{varimp} (variable importance) object}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  Xdat <- subset(cfex, select = -c(W, Y))
#'  X <- make_contrasts(Xdat, 'fct')
#'  cf <- causal_forest(X, cfex$Y, cfex$W)
#'
#'  cf_eval(cf, Xdat)
#' }
cf_eval <- function(fit, Xdat, preds = NULL) {
  res <- results(fit, preds)
  varimp <- varimp(fit)

  validate_cf_eval(new_cf_eval(Xdat, res, varimp))
}


#' Visualize a causal forest evaluation object
#'
#' Many different figures and diagnostic plots can be created. Specify the
#' desired plot using the \code{kind} argument.
#'
#' Possible options for \code{kind} are
#'
#' \describe{
#' \item{\code{cate}}{A density plot of estimated conditional average
#' treatment effects, i.e. the causal forest predictions. The most
#' straightforward way to look for treatment effect heterogeneity.}
#'
#' \item{\code{bias}}{A histogram of each observation's contribution to the
#' overall bias of the model, relative to a simple difference in means.}
#'
#' \item{\code{propensities}}{A histogram of fitted propensities. The causal
#' forest requires the assumption that we cannot deterministically tell the
#' treatment status of an individual given its covariates. In other words, none
#' of the propensity scores should be near zero or one.}
#'
#' \item{\code{balnum, balcat}}{After accounting for propensity, covariate
#' distributions should be balanced between treated and control observations.
#' \code{balnum} plots overlaid histograms, one for treated and one for
#' control, of each numeric covariate. \code{balcat} plots stacked bar charts
#' of the proportions of each categorical covariate.}
#'
#' \item{\code{catecovar}}{A scatter plot of estimated CATEs as a function of a
#' certain covariate.}
#' }
#'
#' @param x A \code{\link{cf_eval}} object
#' @param kind The type of plot to create
#' @param ... Additional arguments passed to subsequent plot functions
#'
#' @return A plot
#' @export
#'
#' @family plotting methods
#' @examples
#' \dontrun{
#'  require(grf)
#'
#'  Xdat <- subset(cfex, select = -c(W, Y))
#'  X <- make_contrasts(Xdat, 'fct')
#'  cf <- causal_forest(X, cfex$Y, cfex$W)
#'
#'  cfe <- cf_eval(cf, Xdat)
#'  plot(cfe, kind = 'cate')
#'  plot(cfe, kind = 'balnum', covar = c('V1', 'V2', 'V3'))
#' }
plot.cf_eval <- function(x, kind, ...) {
  plottype <- match.arg(kind, c('cate', 'bias', 'propensities', 'balnum', 'balcat', 'catecovar'))

  switch(plottype,
         cate = plot(x$res, kind = 'cate', ...),
         bias = plot(x$res, kind = 'bias', ...),
         propensities = plot(x$res, kind = 'propensities', ...),
         balnum = plot_covariate_balance_numeric(x$dat, x$res, ...),
         balcat = plot_covariate_balance_categorical(x$dat, x$res, ...),
         catecovar = plot_predictions_vs_covariate(x$dat, x$res, ...))
}
