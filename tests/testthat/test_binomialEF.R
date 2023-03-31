context("Testing binomialEF")
library(extendedFamily)

################
# Test loglog link
################

loglogFam <- binomialEF(link = "loglog")
cloglogFam <- stats::binomial(link = "cloglog")

test_that("Has all the correct elements", {
  expect_true(all(names(loglogFam) == names(cloglogFam)))
  expect_true(all(class(loglogFam) == class(cloglogFam)))
})

test_that("Use complementary pairs relationship to cloglog to confirm results", {
  expect_true(all(round(loglogFam$linkfun(seq(0, 1, .01)), 10) == round(-1 * cloglogFam$linkfun(seq(1, 0, -.01)), 10)))
  expect_true(all(round(loglogFam$linkinv(seq(-5, 5, .01)), 10) == round(1 - cloglogFam$linkinv(seq(5, -5, -.01)), 10)))
  expect_true(all(round(loglogFam$mu.eta(seq(-5, 5, .01)), 10) == round(cloglogFam$mu.eta(seq(5, -5, -.01)), 10)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(loglogFam$linkinv(loglogFam$linkfun(seq(0, 1, .01))), seq(0, 1, .01))))
  expect_true(isTRUE(all.equal(loglogFam$linkfun(loglogFam$linkinv(seq(0, 1, .01))), seq(0, 1, .01))))
})

test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(loglogFam$mu.eta(seq(-5, 5, .01)), numDeriv::grad(loglogFam$linkinv, seq(-5, 5, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(loglogFam$valideta(seq(-5, 5, .01)))
})

rm(loglogFam, cloglogFam)

data(heart)

model <- glm(
  formula = death ~ anterior + hcabg +
    kk2 + kk3 + kk4 + age2 + age3 + age4,
  data = heart,
  family = binomialEF(link = "loglog")
)

coeff <- summary(model)$coefficients[, 1]
coeff <- round(coeff, 4)
bookExample <- c(-1.699495, .2041431, .2318145, .2523179, .3149235, 1.18085, .104686, .4162827, .6921546)
bookExample <- round(bookExample, 4)

test_that("Confirm link matches example in glm book", {
  expect_true(nrow(heart) > 1)
  expect_true(all(coeff == bookExample))
})

rm(model, coeff, bookExample)

################
# Test logc link
################

logcFam <- binomialEF(link = "logc")
logFam <- stats::binomial(link = "log")

test_that("Has all the correct elements", {
  expect_true(all(names(logcFam) == names(logFam)))
  expect_true(all(class(logcFam) == class(logFam)))
})


test_that("Use relationship to log link to check inverse link", {
  expect_true(all(round(logcFam$linkinv(seq(-5, 5, .01)), 10) == round(1 - logFam$linkinv(seq(-5, 5, .01)), 10)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(logcFam$linkinv(logcFam$linkfun(seq(0, 1, .01))), seq(0, 1, .01))))
  expect_true(isTRUE(all.equal(logcFam$linkfun(logcFam$linkinv(seq(0, 1, .01))), seq(0, 1, .01))))
})

test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(logcFam$mu.eta(seq(-10, 0, .01)), numDeriv::grad(logcFam$linkinv, seq(-10, 0, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(logcFam$valideta(seq(-10, 0, .01)))
  expect_false(logcFam$valideta(seq(.011, 10, .01)))
})

rm(logcFam, logFam)

################
# Test identity link
################

binomIdent <- binomialEF(link = "identity")
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(setequal(names(binomIdent), c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomIdent) == class(gaussIdent)))
})

test_that("Check against gaussian family)", {
  expect_true(all(round(binomIdent$linkfun(seq(0, 1, .01)), 10) == round(gaussIdent$linkfun(seq(0, 1, .01)), 10)))
  expect_true(all(round(binomIdent$linkinv(seq(0, 1, .01)), 10) == round(gaussIdent$linkinv(seq(0, 1, .01)), 10)))
  expect_true(all(round(binomIdent$mu.eta(seq(-5, 5, .01)), 10) == round(gaussIdent$mu.eta(seq(5, -5, -.01)), 10)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(binomIdent$linkinv(binomIdent$linkfun(seq(0, 1, .01))), seq(0, 1, .01))))
  expect_true(isTRUE(all.equal(binomIdent$linkfun(binomIdent$linkinv(seq(0, 1, .01))), seq(0, 1, .01))))
})

test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomIdent$mu.eta(seq(0, 1, .01)), numDeriv::grad(binomIdent$linkinv, seq(0, 1, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(binomIdent$valideta(seq(0, 1, .01)))
  expect_false(binomIdent$valideta(seq(-1, -.01, .01)))
  expect_false(binomIdent$valideta(seq(1.1, 10, .01)))
})

rm(binomIdent, gaussIdent)

################
# Test odds-power link
################

# alpha 5
binomOP <- binomialEF(link = "odds-power", alpha = 5)
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(setequal(names(binomOP), c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomOP) == class(gaussIdent)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(binomOP$linkinv(binomOP$linkfun(seq(0, .99, .01))), seq(0, .99, .01))))
  expect_true(isTRUE(all.equal(binomOP$linkfun(binomOP$linkinv(seq(0, .99, .01))), seq(0, .99, .01))))
})

# Testing over subset of valid range. Getting very close to numerical underflow.
test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(-.18, 10, .01)), numDeriv::grad(binomOP$linkinv, seq(-.18, 10, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(binomOP$valideta(seq(-.20, 10, .01)))
  expect_false(binomOP$valideta(seq(-10, -.21, .01)))
  expect_false(binomOP$valideta(-.40))
})

rm(binomOP, gaussIdent)

# alpha 4
binomOP <- binomialEF(link = "odds-power", alpha = 4)
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(setequal(names(binomOP), c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomOP) == class(gaussIdent)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(binomOP$linkinv(binomOP$linkfun(seq(0, .99, .01))), seq(0, .99, .01))))
  expect_true(isTRUE(all.equal(binomOP$linkfun(binomOP$linkinv(seq(0, .99, .01))), seq(0, .99, .01))))
})

# Testing over subset of valid range. Getting very close to numerical underflow.
test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(.01, 10, .01)), numDeriv::grad(binomOP$linkinv, seq(.01, 10, .01)))))
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(-.24, -.01, .01)), numDeriv::grad(binomOP$linkinv, seq(-.24, -.01, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(binomOP$valideta(seq(.01, 10, .01)))
  expect_true(binomOP$valideta(seq(-.25, -.01, .01)))
  expect_false(binomOP$valideta(seq(-10, -.26, .01)))
  expect_false(binomOP$valideta(0))
})

rm(binomOP, gaussIdent)

# alpha 3
binomOP <- binomialEF(link = "odds-power", alpha = 3)
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(setequal(names(binomOP), c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomOP) == class(gaussIdent)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(binomOP$linkinv(binomOP$linkfun(seq(0, .99, .01))), seq(0, .99, .01))))
  expect_true(isTRUE(all.equal(binomOP$linkfun(binomOP$linkinv(seq(0, .99, .01))), seq(0, .99, .01))))
})

test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(-.33, 10, .01)), numDeriv::grad(binomOP$linkinv, seq(-.33, 10, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(binomOP$valideta(seq(-.33, 10, .01)))
  expect_false(binomOP$valideta(seq(-10, -.34, .01)))
  expect_false(binomOP$valideta(-2 / 3))
})

rm(binomOP, gaussIdent)

# alpha 2
binomOP <- binomialEF(link = "odds-power", alpha = 2)
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(setequal(names(binomOP), c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomOP) == class(gaussIdent)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(binomOP$linkinv(binomOP$linkfun(seq(0, .99, .01))), seq(0, .99, .01))))
  expect_true(isTRUE(all.equal(binomOP$linkfun(binomOP$linkinv(seq(0, .99, .01))), seq(0, .99, .01))))
})

test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(0, 1, .01)), numDeriv::grad(binomOP$linkinv, seq(0, 1, .01)))))
})

# Testing over subset of valie range. Getting very close to numerical underflow.
test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(.01, 10, .01)), numDeriv::grad(binomOP$linkinv, seq(.01, 10, .01)))))
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(-.49, -.01, .01)), numDeriv::grad(binomOP$linkinv, seq(-.49, -.01, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(binomOP$valideta(seq(.01, 10, .01)))
  expect_true(binomOP$valideta(seq(-.50, -.01, .01)))
  expect_false(binomOP$valideta(seq(-10, -.51, .01)))
  expect_false(binomOP$valideta(0))
})

rm(binomOP, gaussIdent)

# alpha 1
binomOP <- binomialEF(link = "odds-power", alpha = 1)
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(setequal(names(binomOP), c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomOP) == class(gaussIdent)))
})

test_that("Use link(inverse_link(X)) = X to check link)", {
  expect_true(isTRUE(all.equal(binomOP$linkinv(binomOP$linkfun(seq(0, .99, .01))), seq(0, .99, .01))))
  expect_true(isTRUE(all.equal(binomOP$linkfun(binomOP$linkinv(seq(0, .99, .01))), seq(0, .99, .01))))
})

# Numerical method seems to not work well around near vertical slopes.
# Differencing method is .999 and Richardson is .5
# Testing a subset of range
test_that("Use numerical methods to check derivative of inverse link.", {
  expect_true(isTRUE(all.equal(binomOP$mu.eta(seq(-.05, 10, .01)), numDeriv::grad(binomOP$linkinv, seq(-.05, 10, .01)))))
})

test_that("Confirm valideta works as expected", {
  expect_true(binomOP$valideta(seq(-1, 10, .01)))
  expect_false(binomOP$valideta(seq(-10, -1.01, .01)))
  expect_false(binomOP$valideta(-2))
})

rm(binomOP, gaussIdent)

################
# Input checking
################
test_that("Confirm input checking works", {
  expect_error(binomialEF(link = c("loglog", "loglog")), "Argument link should have length 1.")
  expect_error(binomialEF(link = 1234), "Argument link should be a character.")
  expect_error(binomialEF(link = c("cloglog")), "Argument link should be 'loglog', 'logc', 'identity', or 'odds-power'.")

  expect_error(binomialEF(link = "odds-power", alpha = c(-1, 1)), "Argument alpha should have length 1.")
  expect_error(binomialEF(link = "odds-power", alpha = "1"), "Argument alpha should be numeric.")
  expect_error(binomialEF(link = "odds-power", alpha = 1.1), "Argument alpha should be a whole number.")
  expect_error(binomialEF(link = "odds-power", alpha = 0), "Argument alpha should be positive.")
})
