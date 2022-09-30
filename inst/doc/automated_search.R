## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
# knitr options
knitr::opts_chunk$set(fig.width=6, fig.height=4.5)

## -----------------------------------------------------------------------------
# libs
library(autohrf)

# load the data
df <- swm
head(df)

## -----------------------------------------------------------------------------
# 2 events: delay, response
model2 <- data.frame(event = c("delay", "response"),
                     start_time = c(0, 10),
                     end_time = c(10, 15))

# 3 events: encoding, delay, response
model3 <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0, 10),
                     end_time = c(0.5, 10, 15))

## -----------------------------------------------------------------------------
model_constraints <- list(model2, model3)
# to speed vignette building we here load results from a previous autohrf run
autofit <- swm_autofit

# in practice you should run
#autofit <- autohrf(df, model_constraints, tr = 2.5)

## -----------------------------------------------------------------------------
best <- get_best_models(autofit)

## -----------------------------------------------------------------------------
plot_best_models(autofit)

