## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(fastgbm)

x <- as.matrix(mtcars[, c("mpg", "disp", "hp", "wt")])
y <- mtcars$am  # 0 = automatic, 1 = manual

fit <- fastgbm(
  x, y = y, objective = "binary",
  ntrees = 100L, learning_rate = 0.1, max_depth = 3L,
  seed = 1L, verbose = FALSE
)
fit

## -----------------------------------------------------------------------------
prob <- predict(fit, x, type = "response")  # predicted probabilities
head(prob)

link <- predict(fit, x, type = "link")      # log-odds
head(link)

metrics(fit, y = y)  # log loss
mean((prob > 0.5) == y)  # training accuracy
importance(fit)

## -----------------------------------------------------------------------------
dat <- mtcars
dat$am <- factor(dat$am)
fit2 <- fastgbm(am ~ mpg + disp + hp + wt, data = dat, ntrees = 100L, verbose = FALSE)

