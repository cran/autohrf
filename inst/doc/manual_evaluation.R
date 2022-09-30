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
# 3 events: encoding, delay, response
model3 <- data.frame(event      = c("encoding", "delay", "response"),
                     start_time = c(0,           2.65,    12.5),
                     duration   = c(2.65,        9.85,    3))

# 4 events: fixation, target, delay, response
model4 <- data.frame(event      = c("fixation", "target", "delay", "response"),
                     start_time = c(0,           2.5,      2.65,    12.5),
                     duration   = c(2.5,         0.1,      9.85,    3))

# 5 events: fixation, target, delay, probe, response
model5 <- data.frame(event      = c("fixation", "target", "delay", "probe", "response"),
                     start_time = c(0,           2.5,      2.65,    12.5,    13),
                     duration   = c(2.5,         0.1,      9.85,    0.5,     2.5))

## -----------------------------------------------------------------------------
# 3 events
em3 <- evaluate_model(df, model3, tr = 2.5)
plot_model(em3)

## -----------------------------------------------------------------------------
# 4 events
em4 <- evaluate_model(df, model4, tr = 2.5)
plot_model(em4)

## -----------------------------------------------------------------------------
em5 <- evaluate_model(df, model5, tr = 2.5)
plot_model(em5)

