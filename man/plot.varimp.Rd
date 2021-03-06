% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varimp.R
\name{plot.varimp}
\alias{plot.varimp}
\title{Plot variable importance of a causal forest}
\usage{
\method{plot}{varimp}(x, ...)
}
\arguments{
\item{x}{A \code{\link{varimp}} object}

\item{...}{Additional parameters (not used)}
}
\value{
A plot
}
\description{
Plot variable importance of a causal forest
}
\examples{
\dontrun{
 require(grf)

 Xdat <- subset(cfex, select = -c(W, Y))
 X <- make_contrasts(Xdat, 'fct')
 cf <- causal_forest(X, cfex$Y, cfex$W)

 cfe <- cf_eval(cf, Xdat)
 plot(cfe$varimp)
}
}
\concept{plotting functions}
