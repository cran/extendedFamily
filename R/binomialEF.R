#' Additional Binomial Links For Generalized Linear Models
#'
#' @param link name of link function. One of loglog, logc, identity, or  odds-power (Default: loglog)
#' @param alpha power for odds-power link. Not used otherwise. (Default: 1)
#' @details
#' family is a generic function with methods for classes "glm" and "lm".
#'
#' The loglog link works well for many datasets. The range of the link is negative
#' infinity to positive infinity. For all other links, this is not true. This can cause a failure to converge in R's
#' glm function. If this happens, the link does not work well for the training data. Try another link.
#'
#' @return An object of class "family" (which has a concise print method). This is a list with elements
#'
#'
#' \itemize{
#'   \item family: character: the family name.
#'   \item link: character: the link name.
#'   \item linkfun: function: the link.
#'   \item linkinv: function: the inverse of the link function.
#'   \item variance: function: the variance as a function of the mean.
#'   \item dev.resids function giving the deviance for each observation as a function of (y, mu, wt),
#'         used by the \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.summaries.html}{residuals}
#'         method when computing deviance residuals.
#'   \item aic: function giving the AIC value if appropriate (but NA for the quasi- families).
#'         More precisely, this function returns -2 ll + 2 s, where ll is the log-likelihood and s
#'         is the number of estimated scale parameters. Note that the penalty term for the location
#'         parameters (typically the “regression coefficients”) is added elsewhere, e.g., in
#'         \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}{glm.fit}()
#'         or \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/AIC.html}{AIC}(),
#'         see the AIC example in \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}{glm}.
#'         See \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/logLik.html}{logLik}
#'         for the assumptions made about the dispersion parameter.
#'   \item initialize: expression. This needs to set up whatever data objects are needed for the family
#'         as well as n (needed for AIC in the binomial family) and mustart (See
#'         \href{https://stat.ethz.ch/R-manual/R-devel/RHOME/library/stats/html/glm.html}{glm})
#'   \item validmu: logical function. Returns TRUE if a mean vector mu is within the domain of variance.
#'   \item valideta: logical function. Returns TRUE if a linear predictor eta is within the domain of linkinv.
#'   \item simulate: (optional) function simulate(object, nsim) to be called by the "lm" method of
#'         \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/simulate.html}{simulate}.
#'         It will normally return a matrix with nsim columns and one row for each fitted value,
#'         but it can also return a list of length nsim. Clearly this will be missing for ‘quasi-’ families.
#'   \item dispersion: (optional for R >= 4.3.0) numeric: value of the dispersion parameter,
#'         if fixed, or NA_real_ if free.
#'         }
#'
#'
#' @examples
#' library(stats)
#' library(extendedFamily)
#'
#' # loglog example
#' data(heart)
#' model <- glm(
#'   formula = death ~ anterior + hcabg +
#'     kk2 + kk3 + kk4 + age2 + age3 + age4,
#'   data = heart,
#'   family = binomialEF(link = "loglog")
#' )
#' @export
binomialEF <- function(link = "loglog", alpha = 1) {
  # Code is a modification of stats's package family implementation.

  assertthat::assert_that(length(link) == 1, msg = "Argument link should have length 1.")
  assertthat::assert_that(is.character(link), msg = "Argument link should be a character.")
  assertthat::assert_that(link %in% c("loglog", "logc", "identity", "odds-power"),
    msg = "Argument link should be 'loglog', 'logc', 'identity', or 'odds-power'."
  )

  if (link == "odds-power") {
    assertthat::assert_that(length(alpha) == 1, msg = "Argument alpha should have length 1.")
    assertthat::assert_that(is.numeric(alpha), msg = "Argument alpha should be numeric.")
    assertthat::assert_that(alpha %% 1 == 0, msg = "Argument alpha should be a whole number.")
    assertthat::assert_that(alpha > 0L, msg = "Argument alpha should be positive.")
  }

  linktemp <- link
  switch(link,
    "loglog" = {
      linkfun <- function(mu) {
        -log(-log(mu))
      }
      linkinv <- function(eta) {
        mu <- pmin(exp(-exp(-eta)), 1 - .Machine$double.eps)
        mu <- pmax(mu, .Machine$double.eps)
        return(mu)
      }
      mu.eta <- function(eta) {
        eta <- pmax(eta, -700)
        dmu <- exp(-exp(-eta)) * exp(-eta)
        dmu <- pmax(dmu, .Machine$double.eps)
        return(dmu)
      }
      valideta <- function(eta) {
        TRUE
      }
    },
    "logc" = {
      linkfun <- function(mu) {
        log(1 - mu)
      }
      linkinv <- function(eta) {
        mu <- pmin(1 - exp(eta), 1 - .Machine$double.eps)
        return(mu)
      }
      mu.eta <- function(eta) {
        dmu <- -exp(eta)
        return(dmu)
      }
      valideta <- function(eta) {
        all(eta <= 0)
      }
    },
    "identity" = {
      linkfun <- stats::gaussian(link = "identity")$linkfun
      linkinv <- stats::gaussian(link = "identity")$linkinv
      mu.eta <- stats::gaussian(link = "identity")$mu.eta
      valideta <- function(eta) {
        all(eta >= 0 & eta <= 1)
      }
    },
    "odds-power" = {
      linkfun <- function(mu) {
        ((mu / (1 - mu))^alpha - 1) / alpha
      }
      linkinv <- function(eta) {
        mu <- ((1 + alpha * eta)^(1 / alpha)) / (1 + (1 + alpha * eta)^(1 / alpha))
        mu <- pmin(mu, 1 - .Machine$double.eps)
        mu <- pmax(mu, .Machine$double.eps)
        return(mu)
      }
      mu.eta <- function(eta) {
        dmu <- (1 + alpha * eta)^((1 - alpha) / alpha) / (1 + (1 + alpha * eta)^(1 / alpha))^2
        return(dmu)
      }
      valideta <- function(eta) {
        all(eta >= -1 / alpha & eta != ((-1)^alpha - 1) / alpha)
      }
    }
  )

  copyMe <- stats::binomial(link = "cloglog")
  variance <- copyMe$variance
  validmu <- copyMe$validmu
  dev.resids <- copyMe$dev.resids
  aic <- copyMe$aic
  initialize <- copyMe$initialize
  simfun <- copyMe$simfun
  dispersion <- copyMe$dispersion

  out <- structure(list(
    family = "binomial",
    link = linktemp,
    linkfun = linkfun,
    linkinv = linkinv,
    variance = variance,
    dev.resids = dev.resids,
    aic = aic,
    mu.eta = mu.eta,
    initialize = initialize,
    validmu = validmu,
    valideta = valideta,
    simulate = simfun
  ),
  class = "family"
  )

  if (!is.null(dispersion)) {
    # R >= 4.3.0
    out$dispersion <- dispersion
  }

  return(out)
}
