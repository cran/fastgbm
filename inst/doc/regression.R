## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(fastgbm)

x <- as.matrix(mtcars[, c("cyl", "disp", "hp", "wt")])
y <- mtcars$mpg

fit <- fastgbm(
  x, y = y, objective = "regression",
  ntrees = 100L, learning_rate = 0.1, max_depth = 3L,
  seed = 1L, verbose = FALSE
)
fit

## -----------------------------------------------------------------------------
pred <- predict(fit, x, type = "response")
head(pred)

metrics(fit, y = y)
importance(fit)

## -----------------------------------------------------------------------------
fit2 <- fastgbm(mpg ~ cyl + disp + hp + wt, data = mtcars, ntrees = 100L, verbose = FALSE)

## -----------------------------------------------------------------------------
set.seed(1)
idx <- sample(nrow(mtcars), 24)
fit3 <- fastgbm(
  x[idx, ], y = y[idx], objective = "regression",
  ntrees = 200L, validation = list(x = x[-idx, ], y = y[-idx]),
  early_stopping = 10L, verbose = FALSE
)
fit3$stopping_reason
fit3$best_iteration

