
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extendedFamily

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/gmcmacran/extendedFamily.svg?branch=master)](https://travis-ci.org/gmcmacran/extendedFamily)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/extendedFamily/branch/master/graph/badge.svg)](https://codecov.io/gh/gmcmacran/extendedFamily?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/extendedFamily)](https://cran.r-project.org/package=extendedFamily)
<!-- badges: end -->

The goal of extendedFamily is to add new links to R’s generalized linear
models. These families are drop in additions to the existing families.

## Installation

You can install the released version of extendedFamily from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("extendedFamily")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gmcmacran/extendedFamily")
```

## Example: loglog link

The heart data contains info on 4,483 heart attack victims. The goal is
to predict if a patient died in the next 48 hours following a myocardial
infarction.

``` r
library(dplyr, warn.conflicts = FALSE)
library(yardstick)
#> For binary classification, the first factor level is assumed to be the event.
#> Set the global option `yardstick.event_first` to `FALSE` to change this.
library(extendedFamily)

data(heart)

heart %>%
  group_by(death) %>%
  summarise(Count = n())
#> # A tibble: 2 x 2
#>   death Count
#>   <dbl> <int>
#> 1     0  4307
#> 2     1   176
```

The low frequency of deaths suggests the loglog link is probably a
better model than a logit link. Lets find out\!

``` r
glmLogit <- glm(formula = death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4, 
                data = heart, family = binomial(link = "logit"))
glmLoglog <- glm(formula = death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4, 
                 data = heart, family = binomialEF(link = "loglog"))
```

Note the minimal code change between the two models. Only the family
changed.

Lets calculate AUC.

``` r
predictions <- heart %>%
  select(death) %>%
  mutate(death = factor(death, levels = c("1", "0")),
         logitProb = predict(object = glmLogit, newdata = heart, type = "response"),
         loglogProb = predict(object = glmLoglog, newdata = heart, type = "response"))

roc_auc(data = predictions, truth = death, logitProb)
#> # A tibble: 1 x 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.797

roc_auc(data = predictions, truth = death, loglogProb)
#> # A tibble: 1 x 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.801
```

A slightly higher AUC was achieved by simply changing the link.
