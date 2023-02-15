## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
# knitr options
knitr::opts_chunk$set(fig.width=6, fig.height=4.5)

## -----------------------------------------------------------------------------
# libraries
library(autohrf)
library(dplyr)
library(ggplot2)
library(magrittr)

# load the data
df <- swm
head(df)

## -----------------------------------------------------------------------------
# prepare relevant ROIs for visualization
roisv <- c("R_V1", "R_V4", "R_FEF", "R_AIP", "R_POS1", "R_A1")

# view data of selected ROIs
df %>% 
  filter(roi %in% roisv) %>%
  mutate(roi = factor(roi, levels=roisv)) %>%
  ggplot(aes(t, y)) + 
  geom_line(size=0.8) +
  facet_wrap(~ roi, nrow = 1)

## -----------------------------------------------------------------------------
# prepare models
model1 <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0.15, 10),
                     duration = c(0.15, 9.85, 3))

model2 <- data.frame(event = c("encoding", "early_delay", "late_delay", "response"),
                     start_time = c(0, 0.15, 5, 10),
                     duration = c(0.15, 4.85, 5, 3))

model3 <- data.frame(event = c("encoding", "delay", "probe", "response"),
                     start_time = c(0, 0.15, 10, 10.5),
                     duration = c(0.15, 9.85, 0.5, 2.5))

model4 <- data.frame(event = c("stimulus", "encoding", "delay", "probe", "response"),
                     start_time = c(0, 0.15, 2, 10, 10.5),
                     duration = c(0.15, 1.85, 8, 0.5, 2.5))

# evaluate models
em1 <- evaluate_model(df, model1, tr = 1, hrf = "spm")
em2 <- evaluate_model(df, model2, tr = 1, hrf = "spm")
em3 <- evaluate_model(df, model3, tr = 1, hrf = "spm")
em4 <- evaluate_model(df, model4, tr = 1, hrf = "spm")

# plot model fits
plot_model(em1, by_roi = TRUE, rois = roisv, nrow = 1)
plot_model(em2, by_roi = TRUE, rois = roisv, nrow = 1)
plot_model(em3, by_roi = TRUE, rois = roisv, nrow = 1)
plot_model(em4, by_roi = TRUE, rois = roisv, nrow = 1)

## -----------------------------------------------------------------------------
# prepare relevant ROIs for autohrf
roisa <- c("R_V1", "R_V2", "R_V3", "R_V4", "R_IPS1", "R_4", "R_3a", "R_3b", 
            "R_1", "R_2", "R_6mp", "R_6ma", "R_SCEF", "R_6d", "R_6a", "R_FEF",
            "R_6v", "R_6r", "R_PEF", "R_LIPv", "R_LIPd", "R_VIP", "R_AIP", 
            "R_MIP", "R_7PC", "R_7AL", "R_7Am", "R_7PL", "R_7Pm", "R_PFm", 
            "R_PF", "R_PFt", "R_PFop", "R_IP0", "R_IP1", "R_IP2", "R_p9-46v", 
            "R_a9-46v", "R_46", "R_9-46d", "L_V1", "L_V2", "L_V3", "L_V4", 
            "L_IPS1", "L_4", "L_3a", "L_3b", "L_1", "L_2", "L_6mp", "L_6ma", 
            "L_SCEF", "L_6d", "L_6a", "L_FEF", "L_6v", "L_6r", "L_PEF", 
            "L_LIPv", "L_LIPd", "L_VIP", "L_AIP", "L_MIP", "L_7PC", "L_7AL", 
            "L_7Am", "L_7PL", "L_7Pm", "L_PFm", "L_PF", "L_PFt", "L_PFop", 
            "L_IP0", "L_IP1", "L_IP2", "L_p9-46v",  
            "L_a9-46v", "L_46", "L_9-46d")

# extract data for autohrf
dfa <- df %>%
  filter(roi %in% roisa)

## -----------------------------------------------------------------------------
# Model 1 with events encoding, delay, and response

# prepare model constraints
modelA <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0.15, 10),
                     end_time = c(0.15, 10, 13),
                     min_duration = c(0.15, 9.85, 3),
                     max_duration = c(0.15, 9.85, 3))

modelB <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0.15, 10),
                     end_time = c(0.15, 10, 13),
                     min_duration = c(0.05, 5, 1.5),
                     max_duration = c(0.15, 9.85, 3))

modelC <- data.frame(event = c("encoding", "delay", "response"),
                     start_time = c(0, 0, 9),
                     end_time = c(1, 11, 14),
                     min_duration = c(0.05, 5, 1.5),
                     max_duration = c(1, 11, 5))

# join models
models <- list(modelA, modelB, modelC)

# run autohrf (to speed vignette building we load results from a previous autohrf run)
# autofit1 <- autohrf(dfa, models, tr = 1, iter = 500, allow_overlap = TRUE)
autofit1 <- swm_autofit1

# plot models' fitness
plot_fitness(autofit1) +
  scale_color_brewer(labels=c("theoretical", "strict", "permissive"), type = "qual", palette = "Set1")

# plot regressors
plot_best_models(autofit1)

# return derived parameters
best_models <- get_best_models(autofit1)

# evaluate models
modelA <- best_models[[1]]
emA <- evaluate_model(df, modelA, tr = 1, hrf = "spm")
plot_model(emA, by_roi = TRUE, rois = roisv, nrow = 1)

modelB <- best_models[[2]]
emB <- evaluate_model(df, modelB, tr = 1, hrf = "spm")
plot_model(emB, by_roi = TRUE, rois = roisv, nrow = 1)

modelC <- best_models[[3]]
emC <- evaluate_model(df, modelC, tr = 1, hrf = "spm")
plot_model(emC, by_roi = TRUE, rois = roisv, nrow = 1)

# Model 2 with events encoding, early delay, late delay & response

# prepare models
modelA <- data.frame(event = c("encoding", "early_delay", "late_delay", "response"),
                     start_time = c(0, 0.15, 5, 10),
                     end_time = c(0.15, 5, 10, 13),
                     min_duration = c(0.15, 4.85, 5, 3),
                     max_duration = c(0.15, 4.85, 5, 3))

modelB <- data.frame(event = c("encoding", "early_delay", "late_delay", "response"),
                     start_time = c(0, 0.15, 5, 10),
                     end_time = c(0.15, 5, 10, 13),
                     min_duration = c(0.05, 2.5, 2.5, 1.5),
                     max_duration = c(0.15, 4.85, 5, 3))

modelC <- data.frame(event = c("encoding", "early_delay", "late_delay", "response"),
                     start_time = c(0, 0, 4, 9),
                     end_time = c(1, 6, 11, 14),
                     min_duration = c(0.05, 2.5, 2.5, 1.5),
                     max_duration = c(1, 6, 6, 5))

# join models
models <- list(modelA, modelB, modelC)

# run autohrf (to speed vignette building we load results from a previous autohrf run)
# autofit2 <- autohrf(dfa, models, tr = 1, iter = 200, allow_overlap = TRUE)
autofit2 <- swm_autofit2

# plot models" fitness
plot_fitness(autofit2) +
  scale_color_brewer(labels=c("theoretical", "strict", "permissive"), type = "qual", palette = "Set1")

# plot regressors
plot_best_models(autofit2)

# return derived parameters
best_models <- get_best_models(autofit2)

# evaluate models
modelA <- best_models[[1]]
emA <- evaluate_model(df, modelA, tr = 1, hrf = "spm")
plot_model(emA, by_roi = TRUE, rois = roisv, nrow = 1)

modelB <- best_models[[2]]
emB <- evaluate_model(df, modelB, tr = 1, hrf = "spm")
plot_model(emB, by_roi = TRUE, rois = roisv, nrow = 1)

modelC <- best_models[[3]]
emC <- evaluate_model(df, modelC, tr = 1, hrf = "spm")
plot_model(emC, by_roi = TRUE, rois = roisv, nrow = 1)

