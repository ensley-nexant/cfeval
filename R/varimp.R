new_varimp <- function(v) {
  structure(
    v,
    class = c('varimp', 'tbl_df', 'tbl', 'data.frame')
  )
}

validate_varimp <- function(x) {
  if (!tibble::is_tibble(x)) {
    stop(
      "Varimp must be in a tibble.",
      call. = F
    )
  }

  x
}

#' Create a causal forest variable importance object
#'
#' @param fit A trained \code{\link[grf]{causal_forest}} object
#'
#' @return An object of class \code{varimp}, essentially just a
#'   \link[tibble:tibble-package]{tibble} with a row for each covariate.
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
#'  cf_eval(cf, Xdat)$varimp
#' }
varimp <- function(fit) {
  v <- tibble::tibble(
    var = colnames(as.data.frame(fit$X.orig)),
    importance = grf::variable_importance(fit)[, 1]
  )

  validate_varimp(new_varimp(v))
}


#' Plot variable importance of a causal forest
#'
#' @param x A \code{\link{varimp}} object
#' @param ... Additional parameters (not used)
#'
#' @family plotting functions
#' @return A plot
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
#'  cfe <- cf_eval(cf, Xdat)
#'  plot(cfe$varimp)
#' }
plot.varimp <- function(x, ...) {
  var <- importance <- NULL

  x %>%
    dplyr::mutate(var = forcats::fct_reorder(var, importance)) %>%
    ggplot2::ggplot(ggplot2::aes(x = importance, y = var)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = 'Variable Importance',
                  subtitle = 'Relative frequency of each variable appearing in splits near the root of the trees',
                  x = 'Importance',
                  y = 'Variable')
}
