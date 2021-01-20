extendedFamily
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/extendedFamily)](https://cran.r-project.org/package=extendedFamily)
[![R build
status](https://github.com/gmcmacran/extendedFamily/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/extendedFamily/actions)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/extendedFamily/branch/master/graph/badge.svg)](https://codecov.io/gh/gmcmacran/extendedFamily?branch=master)
<!-- badges: end -->

extendedFamily adds new links to R’s generalized linear models. These
families are drop in additions to the existing families.

## Logit vs Loglog: Mathematical Comparison

The generalized linear model is

  
![ g(Y) = B\_0 + B\_1X\_1
](https://latex.codecogs.com/png.latex?%20g%28Y%29%20%3D%20B_0%20%2B%20B_1X_1%20
" g(Y) = B_0 + B_1X_1 ")  

with g being a link function. For the binomial family, the link is
usually the logit which makes the model

  
![ ln(\\frac{P(Y = 1)}{1 - P(Y = 1)}) = B\_0 + B\_1X\_1
](https://latex.codecogs.com/png.latex?%20ln%28%5Cfrac%7BP%28Y%20%3D%201%29%7D%7B1%20-%20P%28Y%20%3D%201%29%7D%29%20%3D%20B_0%20%2B%20B_1X_1%20
" ln(\\frac{P(Y = 1)}{1 - P(Y = 1)}) = B_0 + B_1X_1 ")  

Using the loglog link, the model is

  
![ -ln(-ln(P(Y = 1))) = B\_0 + B\_1X\_1
](https://latex.codecogs.com/png.latex?%20-ln%28-ln%28P%28Y%20%3D%201%29%29%29%20%3D%20B_0%20%2B%20B_1X_1%20
" -ln(-ln(P(Y = 1))) = B_0 + B_1X_1 ")  

The loglog model assigns a lower probability for X ranging from -5 to 2.
The biggest differences are around -1. For X over 2, the models are
essentially indistinguishable.
<img src="man/figures/README-graphExample-1.png" width="100%" />

## Logit vs Loglog: Model Performance on Real World Data

The heart data contains info on 4,483 heart attack victims. The goal is
to predict if a patient died in the next 48 hours following a myocardial
infarction. The low frequency of deaths suggests the loglog link is
probably a better than the logit link.

``` r
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

Only the family object needs to change to use the loglog link.

``` r
glmLogit <- glm(formula = death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4, 
                data = heart, family = binomial(link = "logit"))
glmLoglog <- glm(formula = death ~ anterior + hcabg + kk2 + kk3 + kk4 + age2 + age3 + age4, 
                 data = heart, family = binomialEF(link = "loglog"))
```

Given the same data, AUC is slightly higher for the loglog link.

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
