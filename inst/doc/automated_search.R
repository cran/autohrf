## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
# knitr options
knitr::opts_chunk$set(fig.width=6, fig.height=4.5)

## -----------------------------------------------------------------------------
# libraries
library(autohrf)

# load the data
df <- swm
head(df)

## -----------------------------------------------------------------------------
# model constraints for three event predictors
model1 <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0.15, 10),
                     end_time = c(0.15, 10, 13))

# model constraints for four event predictors
model2 <- data.frame(event = c("encoding", "early_delay", "late_delay", "response"),
                     start_time = c(0, 0.15, 5, 10),
                     end_time = c(0.15, 5, 10, 13))

# join different model constraints
models <- list(model1, model2)

## -----------------------------------------------------------------------------
# to speed vignette building we here load results from a previous autohrf run
autofit <- swm_autofit

# in practice you should run
# autofit <- autohrf(df, models, tr = 1, population = 10, iter = 10)

## -----------------------------------------------------------------------------
# plot models' fitness across iterations
plot_fitness(autofit)

## -----------------------------------------------------------------------------
# return automatically derived parameters
best <- get_best_models(autofit)

## -----------------------------------------------------------------------------
# visualize automatically derived parameters
plot_best_models(autofit)

