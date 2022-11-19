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
# a model with three event predictors
model1 <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0.15, 10),
                     duration = c(0.15, 9.85, 3))

# a model with four event predictors
model2 <- data.frame(event = c("encoding", "delay", "probe", "response"),
                     start_time = c(0, 0.15, 10, 10.5),
                     duration = c(0.15, 9.85, 0.5, 2.5))

# a model with five event predictors
model3 <- data.frame(event = c("stimulus", "encoding", "delay", "probe", "response"),
                     start_time = c(0, 0.15, 2, 10, 10.5),
                     duration = c(0.15, 1.85, 8, 0.5, 2.5))

## -----------------------------------------------------------------------------
# evaluate models
em1 <- evaluate_model(df, model1, tr = 1)
em2 <- evaluate_model(df, model2, tr = 1)
em3 <- evaluate_model(df, model3, tr = 1)

## -----------------------------------------------------------------------------
# plot models fit to the data
plot_model(em1)
plot_model(em2)
plot_model(em3)

