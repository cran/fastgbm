## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(fastgbm)
library(survival)

lung_dat <- na.omit(lung[, c("time", "status", "age", "sex", "ph.ecog")])
x <- as.matrix(lung_dat[, c("age", "sex", "ph.ecog")])

fit <- fastgbm(
  x,
  time = lung_dat$time,
  status = lung_dat$status,
  objective = "cox",
  ntrees = 100L,
  learning_rate = 0.05,
  max_depth = 3L,
  seed = 1L,
  verbose = FALSE
)
fit

## -----------------------------------------------------------------------------
# Linear predictor (log relative risk)
lp <- predict(fit, x, type = "link")
head(lp)

# Survival probabilities at specific horizons
predict(fit, x[1:5, ], type = "survival", times = c(90, 180, 365))

## -----------------------------------------------------------------------------
fit2 <- fastgbm(Surv(time, status) ~ age + sex + ph.ecog, data = lung_dat, ntrees = 100L, verbose = FALSE)

## -----------------------------------------------------------------------------
metrics(fit, y = Surv(lung_dat$time, lung_dat$status))
importance(fit)

## ----fig.width = 5, fig.height = 3.5------------------------------------------
pd <- pdp(fit, "age", data = as.data.frame(x), grid_resolution = 15)
plot(pd)

