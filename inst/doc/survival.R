## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(fastgbm)
library(survival)

## -----------------------------------------------------------------------------
lung_dat <- na.omit(lung[, c("time", "status", "age", "sex", "ph.ecog")])
x <- as.matrix(lung_dat[, c("age", "sex", "ph.ecog")])

cox_fit <- fastgbm(x, time = lung_dat$time, status = lung_dat$status,
                   objective = "cox", ntrees = 100L, max_depth = 3L, verbose = FALSE)

risk <- predict(cox_fit, x, type = "link")
surv <- predict(cox_fit, x[1:5, ], type = "survival", times = c(90, 180, 365))
surv

## -----------------------------------------------------------------------------
set.seed(1)
n <- 200
x_aft <- matrix(rnorm(n * 2), ncol = 2, dimnames = list(NULL, c("x1", "x2")))
time <- exp(1 + 0.6 * x_aft[, 1] - 0.3 * x_aft[, 2] + rnorm(n, sd = 0.3))
status <- rbinom(n, 1, 0.75)

aft_fit <- fastgbm(x_aft, time = time, status = status, objective = "aft",
                   ntrees = 100L, max_depth = 3L, verbose = FALSE)
predict(aft_fit, x_aft[1:5, ], type = "survival", times = c(1, 3, 5))

## -----------------------------------------------------------------------------
pexp_fit <- fastgbm(x, time = lung_dat$time, status = lung_dat$status,
                    objective = "pexp", ntrees = 100L, max_depth = 3L, verbose = FALSE)
predict(pexp_fit, x[1:5, ], type = "survival", times = c(90, 180, 365))
metrics(pexp_fit, y = Surv(lung_dat$time, lung_dat$status))

## -----------------------------------------------------------------------------
metrics(cox_fit, y = Surv(lung_dat$time, lung_dat$status))
metrics(aft_fit, y = Surv(time, status))

## -----------------------------------------------------------------------------
x_missing <- x
x_missing[1, "ph.ecog"] <- NA
predict(cox_fit, x_missing[1:3, ], type = "link")

