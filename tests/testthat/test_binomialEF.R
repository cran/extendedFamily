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

test_that("use complementary pairs relationship to cloglog to confirm results", {
  expect_true(all(round(loglogFam$linkfun(seq(0, 1, .01)), 10) == round(-1 * cloglogFam$linkfun(seq(1, 0, -.01)), 10)))
  expect_true(all(round(loglogFam$linkinv(seq(-5, 5, .01)), 10) == round(1 - cloglogFam$linkinv(seq(5, -5, -.01)), 10)))
  expect_true(all(round(loglogFam$mu.eta(seq(-5, 5, .01)), 10) == round(cloglogFam$mu.eta(seq(5, -5, -.01)), 10)))
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


test_that("use relationship to log link to check inverse link", {
  expect_true(all(round(logcFam$linkinv(seq(-5, 5, .01)), 10) == round(1 - logFam$linkinv(seq(-5, 5, .01)), 10)))
})

test_that("use link(inverse_link(X) = X to check link)", {
  expect_true(all(all.equal(logcFam$linkinv(logcFam$linkfun(seq(0, 1, .1))), seq(0, 1, .1))))
  expect_true(all(all.equal(logcFam$linkfun(logcFam$linkinv(seq(0, 1, .1))), seq(0, 1, .1))))
})

rm(logcFam, logFam)

################
# Test identity link
################

binomIdent <- binomialEF(link = "identity")
gaussIdent <- gaussian(link = "identity")

test_that("Has all the correct elements", {
  expect_true(all(names(binomIdent) == c(names(gaussIdent), "simulate")))
  expect_true(all(class(binomIdent) == class(gaussIdent)))
})

test_that("use link(inverse_link(X) = X to check link)", {
  expect_true(all(round(binomIdent$linkfun(seq(0, 1, .01)), 10) == round(gaussIdent$linkfun(seq(0, 1, .01)), 10)))
  expect_true(all(round(binomIdent$linkinv(seq(0, 1, .01)), 10) == round(gaussIdent$linkinv(seq(0, 1, .01)), 10)))
  expect_true(all(round(binomIdent$mu.eta(seq(-5, 5, .01)), 10) == round(gaussIdent$mu.eta(seq(5, -5, -.01)), 10)))
})

rm(binomIdent, gaussIdent)

################
# Input checking
################
test_that("Confirm input checking works", {
  expect_error(binomialEF(link = c("loglog", "loglog")))
  expect_error(binomialEF(link = 1234))
  expect_error(binomialEF(link = c("cloglog")))
})
