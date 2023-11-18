extendedFamily
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/gmcmacran/extendedFamily/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gmcmacran/extendedFamily/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/extendedFamily/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gmcmacran/extendedFamily?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/extendedFamily)](https://CRAN.R-project.org/package=extendedFamily)
<!-- badges: end -->

extendedFamily adds new links to R’s generalized linear models. These
families are drop in additions to existing families.

Links:

- loglog
- logc
- identity
- odds-power

## Logit and Loglog: Mathematical Comparison

For the binomial family, the link is usually the logit but there are
other options. The loglog model assigns a lower probability for X
ranging from -5 to 2. For X over 2, the models are essentially
indistinguishable. This can lead to improved performance when the
response rate is much lower than 50%.

<img src="man/figures/README-graphExample-1.png" width="100%" />

## Logit and Loglog: Model Performance on Real World Data

The heart data contains info on 4,483 heart attack victims. The goal is
to predict if a patient died in the next 48 hours following a myocardial
infarction. The low death rate makes this dataset a good candidate for
the loglog link.

``` r
data(heart)

heart %>%
  summarise(deathRate = mean(death))
#>    deathRate
#> 1 0.03925942
```

Only the family object needs to change to use the loglog link.

``` r
glmLogit <- glm(
  formula = death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4,
  data = heart, family = binomial(link = "logit")
)
glmLoglog <- glm(
  formula = death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4,
  data = heart, family = binomialEF(link = "loglog")
)
```

AUC improved by changing the link.

``` r
predictions <- heart %>%
  select(death) %>%
  mutate(
    death = factor(death, levels = c("0", "1")),
    logitProb = predict(object = glmLogit, newdata = heart, type = "response"),
    loglogProb = predict(object = glmLoglog, newdata = heart, type = "response")
  )

roc_auc(data = predictions, truth = death, event_level = "second", logitProb)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.797

roc_auc(data = predictions, truth = death, event_level = "second", loglogProb)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.801
```

## Tidymodels

The family objects integrate with Tidymodels.

``` r
library(tidymodels)

heart <- heart %>%
  mutate(death = factor(death, levels = c("0", "1")))

parsnip_fit <-
  logistic_reg() %>%
  set_engine("glm", family = binomialEF("loglog")) %>%
  fit(death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4, data = heart)

testPredictions <- parsnip_fit %>%
  predict(new_data = heart, type = "prob")
testPredictions <- heart %>%
  select(death) %>%
  bind_cols(testPredictions)

testPredictions %>%
  roc_auc(truth = death, event_level = "second", .pred_1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.801
```
