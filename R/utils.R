#' Gather causal forest outputs into a data frame
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
#'  n <- 2000; p <- 10
#'
#'  X <- matrix(rnorm(n * p), n, p)
#'  W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#'  Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#'  cf <- causal_forest(X, Y, W)
#'  preds <- predict(cf, estimate.variance = T)
#'
#'  tidy_cf(cf, preds)
#' }
#'
#' @seealso \url{https://grf-labs.github.io/grf/articles/diagnostics.html#assessing-fit}
#' for a discussion of the bias measure and how it is calculated.
tidy_cf <- function(fit, preds = NULL) {
  predict <- W <- W.hat <- Y.hat <- cate <- Y.hat.0 <- Y.hat.1 <- NULL
  if (is.null(preds)) preds <- stats::predict(fit, estimate.variance = T)

  results <- dplyr::tibble(
    W = fit$W.orig,
    W.hat = fit$W.hat,
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

  results
}
