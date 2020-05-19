new_results <- function(res) {
  structure(
    res,
    class = c('results', 'tbl_df', 'tbl', 'data.frame')
  )
}

validate_results <- function(x) {
  if (!tibble::is_tibble(x)) {
    stop(
      "Results must be in a tibble.",
      call. = F
    )
  }

  x
}

#' Create a causal forest results object
#'
#' Having the out-of-bag prediction results in a tidy, tabular format makes
#' visualization much easier.
#'
#' \code{debiased.error} and \code{excess.error} serve to partition the overall prediction
#' error into two parts. \code{debiased.error} is "irreducible" error in a sense because it
#' cannot be made smaller by increasing the number of trees in the forest. \code{excess.error}
#' can, however. The \link[grf:predict.causal_forest]{grf authors recommend} growing
#' enough trees that \code{excess.error} becomes negligible.
#'
#' @param fit A trained causal forest object from
#'   \code{\link[grf]{causal_forest}}
#' @param preds Out-of-bag training predictions from \code{fit}, If omitted,
#'   they will be generated, but this will slow down the function significantly.
#'
#' @return A \code{\link[dplyr:reexports]{tibble}} containing the following columns:
#'   \describe{
#'   \item{\code{W}}{The original treatment assignments.}
#'   \item{\code{W.hat}}{The estimated treatment propensities:
#'   \eqn{\hat{W} = E[W | X]}{W.hat = E[W | X]}.}
#'   \item{\code{Y}}{The original outcome variable.}
#'   \item{\code{Y.hat}}{The expected response estimates, marginalized over
#'   treatment: \eqn{\hat{Y} = E[Y | X]}{Y.hat = E[Y | X]}.}
#'   \item{\code{treatment}}{The treatment
#'   assignments as a factor, "Control" or "Treated". This looks better in plots
#'   than \code{W} does.}
#'   \item{\code{cate}}{The conditional average treatment effect (CATE) estimates}
#'   \item{\code{cate.se}}{The standard errors of the CATEs.}
#'   \item{\code{debiased.error}}{An estimate of the error obtained if the forest had
#'   an infinite number of trees.}
#'   \item{\code{excess.error}}{A jackknife estimate of how unstable the estimates are
#'   if forests of the same size were repeatedly grown on the same data set.}
#'   \item{\code{IPW}}{The inverse propensity weights: \eqn{\frac{1}{\hat{W}}}{1 / W.hat}
#'   if \eqn{W = 1}, \eqn{\frac{1}{1 - \hat{W}}}{1 / (1 - W.hat)} otherwise.}
#'   \item{\code{bias}}{A measure of each observation's contribution to the overall
#'   bias of the model, relative to a simple difference in means.}
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
#' cf_eval(cf, Xdat)$res
#' }
#'
#' @seealso \url{https://grf-labs.github.io/grf/articles/diagnostics.html#assessing-fit}
#' for a discussion of the bias measure and how it is calculated.
results <- function(fit, preds = NULL) {
  predict <- W <- W.hat <- Y.hat <- cate <- Y.hat.0 <- Y.hat.1 <- NULL
  if (is.null(preds)) preds <- stats::predict(fit, estimate.variance = T)

  res <- dplyr::tibble(
    W = fit$W.orig,
    W.hat = fit$W.hat,
    Y = fit$Y.orig,
    Y.hat = fit$Y.hat,
    treatment = factor(W, levels = c(0, 1), labels = c('Control', 'Treated')),
    cate = preds$predictions,
    cate.se = sqrt(preds$variance.estimates),
    debiased.error = preds$debiased.error,
    excess.error = preds$excess.error,
    IPW = dplyr::if_else(W == 1, 1 / W.hat, 1 / (1 - W.hat)),
    Y.hat.0 = Y.hat - W.hat * cate,
    Y.hat.1 = Y.hat + (1 - W.hat) * cate,
    bias = (W.hat - mean(W)) * (mean(W) * (Y.hat.0 - mean(Y.hat.0)) +
                                  (1-mean(W)) * (Y.hat.1 - mean(Y.hat.1)))
  ) %>%
    dplyr::select(-Y.hat.0, -Y.hat.1)

  validate_results(new_results(res))
}



#' Visualize a causal forest results object
#'
#' Certain plots only require a \code{\link{results}} object and not the other
#' components of a \code{\link{cf_eval}} object. These can be created from
#' \code{results} only.
#'
#' Possible options for \code{kind} are:
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
#' }
#'
#' @param x A \code{\link{results}} object
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
#'  plot(cfe$res, kind = 'bias')
#' }
plot.results <- function(x, kind, ...) {
  type <- match.arg(kind, c('cate', 'bias', 'propensities'))

  switch(type,
         cate = plot_cate(x),
         bias = plot_bias(x),
         propensities = plot_propensities(x))
}
