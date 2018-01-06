#' Optimizes variance
#'
#' Finds the prior parameter that maximizes the marginal likelihood given
#' the prediction.
#'
#' \code{calc.a} returns a prior alpha parameter assuming constant
#' coefficient of variation. \code{calc.b} returns a prior beta parameter
#' assuming constant Fano factor. \code{calc.k} returns a prior variance
#' parameter assuming constant variance.
#'
#' @param y A vector of observed gene counts.
#'
#' @param mu A vector of predictions from \code{\link{expr.predict}}.
#'
#' @param sf Vector of normalized size factors.
#'
#' @return A vector with the optimized parameter and the negative
#' log-likelihood.
#'
#'
#' @importFrom  stats optimize ppoints uniroot var
#' @rdname optimize_variance
#' @export
calc.a <- function(y, mu, sf) {
  n <- length(y)
  if (length(mu) == 1) {
    mu <- rep(mu, n)
  }
  if (length(sf) == 1) {
    sf <- rep(sf, n)
  }
  a.vec <- optimize(calc.loglik.a, interval = c(0, var(y/sf)/mean(y/sf)^2),
                    y = y, mu = mu, sf = sf)
  a <- a.vec$minimum
  a.loglik <- a.vec$objective
  return(c(1/a, a.loglik))
}

#' @rdname optimize_variance
#' @export
calc.b <- function(y, mu, sf) {
  n <- length(y)
  if (length(mu) == 1) {
    mu <- rep(mu, n)
  }
  if (length(sf) == 1) {
    sf <- rep(sf, n)
  }
  if (sum(mu) == 0) {
    return(c(0, 0))
  }
  b.vec <- optimize(calc.loglik.b, interval = c(0, var(y/sf)/mean(y/sf)), y = y, mu = mu,
                    sf = sf)
  b <- b.vec$minimum
  b.loglik <- b.vec$objective
  return(c(1/b, b.loglik))
}

#' @rdname optimize_variance
#' @export
calc.k <- function(y, mu, sf) {
  n <- length(y)
  if (length(mu) == 1) {
    mu <- rep(mu, n)
  }
  if (length(sf) == 1) {
    sf <- rep(sf, n)
  }
  k.vec <- optimize(calc.loglik.k, interval = c(0, var(y/sf)), y = y, mu = mu,
                    sf = sf)
  k <- k.vec$minimum
  k.loglik <- k.vec$objective
  return(c(k, k.loglik))
}
