## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
# knitr options
knitr::opts_chunk$set(fig.width=6, fig.height=4.5)

## -----------------------------------------------------------------------------
# libraries
library(tidyverse)
library(autohrf)

# load data
df <- flanker
head(df)

## -----------------------------------------------------------------------------
# visualize the data
df %>%
    mutate(roi = factor(roi)) %>%
    ggplot(aes(x = t, y = y, color = roi)) +
    geom_line() +
    facet_grid(roi ~ .)

## -----------------------------------------------------------------------------
# setup model constraints
modelS <- data.frame(event        = c("start", "task", "rest"),
                     start_time   = c(      0,       0,     60),
                     end_time     = c(      2,      60,     65),
                     min_duration = c(    0.1,      55,    0.1))

modelP <- data.frame(event        = c("start", "task", "rest"),
                     start_time   = c(      0,       0,     55),
                     end_time     = c(      5,      65,     65),
                     min_duration = c(    0.1,      50,    0.1))

# join models
models <- list(modelS, modelP)

## -----------------------------------------------------------------------------
# optimization settings
tr         <- 2.5
population <- 100
iter       <- 1000
hrf        <- "spm"

# cluster weights
w <- data.frame(roi    = c(   1,    2,    3,    4,    5,    6),
                weight = c(0.14, 0.06, 0.25, 0.15, 0.22, 0.19))

# run optimization (to speed vignette building we load results from a previous autohrf run)
# autofit <- autohrf(df, models, tr=tr, hrf=hrf, population=population, iter=iter, roi_weights=w)
autofit <- flanker_autofit

## -----------------------------------------------------------------------------
# check the convergence of model fitness
plot_fitness(autofit)

# return derived parameters
best_models <- get_best_models(autofit)

# evaluate models
emS <- evaluate_model(df, best_models[[1]], tr=tr, hrf="spm", roi_weights=w)
emP <- evaluate_model(df, best_models[[2]], tr=tr, hrf="spm", roi_weights=w)

# plot model fits
plot_model(emS)
plot_model(emP)

# plot model fits for individual ROI clusters
plot_model(emS, by_roi = TRUE, nrow=6)
plot_model(emP, by_roi = TRUE, nrow=6)

## -----------------------------------------------------------------------------
# prepare models
model1 <- data.frame(event      = c("task"),
                      start_time = c(0),
                      duration   = c(60))

model2 <- data.frame(event      = c("task", "start", "rest"),
                      start_time = c(     0,       0,     60),
                      duration   = c(    60,       1,      1))

model3 <- data.frame(event      = c("task", "start", "rest"),
                      start_time = c(  3.00,    0.00,  60.00),
                      duration   = c( 54.70,    0.18,   2.53))

# model settings
tr  <- 2.5
hrf <- "spm"

# evaluate models
em1 <- evaluate_model(df, model1, tr=tr, hrf=hrf, roi_weights = w)
em2 <- evaluate_model(df, model2, tr=tr, hrf=hrf, roi_weights = w)
em3 <- evaluate_model(df, model3, tr=tr, hrf=hrf, roi_weights = w)

# plot model fits
plot_model(em1, by_roi=TRUE, ncol=1) + facet_grid(roi ~ ., scales="free_y")
plot_model(em2, by_roi=TRUE, ncol=1) + facet_grid(roi ~ ., scales="free_y")
plot_model(em3, by_roi=TRUE, ncol=1) + facet_grid(roi ~ ., scales="free_y")

