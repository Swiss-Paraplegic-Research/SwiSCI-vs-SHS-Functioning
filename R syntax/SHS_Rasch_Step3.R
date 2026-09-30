################ Title: Rasch Analysis and DIF - Swiss Health Survey (SHS) ###
################ Author: Cristina Ehrmann ##################################

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(car)
library(polycor)
library(admisc)
library(mirt)
library(lordif)
library(nFactors)
library(foreign)
library(Hmisc)
library(psych)
library(GPArotation)
library(cowplot)
library(ggplot2)
library(here)

# -----------------------------------------------------------------------------
# Reading in Data (Using relative paths via 'here')
# -----------------------------------------------------------------------------
data_SHS_all_waves <- read.csv(
  file = here("00_Data", "data_SHS_rasch_allwaves_persons.csv"), 
  header = TRUE, 
  dec = ",", 
  sep = ";", 
  stringsAsFactors = FALSE
)

summary(data_SHS_all_waves$Changing_body)

# -----------------------------------------------------------------------------
# Data Recoding & Preparation
# -----------------------------------------------------------------------------
recode <- car::recode

data_SHS_all_waves$seeing <- recode(data_SHS_all_waves$seeing, "1=0;2=1;3=1;4=2", as.factor = FALSE)
data_SHS_all_waves$conversation <- recode(data_SHS_all_waves$conversation, "1=0;2=1;3=2;4=2", as.factor = FALSE)
data_SHS_all_waves$speak <- recode(data_SHS_all_waves$speak, "1=0;2=1;3=1;4=1", as.factor = FALSE)
data_SHS_all_waves$walk <- recode(data_SHS_all_waves$walk, "1=0;2=1;3=1;4=1", as.factor = FALSE)
data_SHS_all_waves$meals <- recode(data_SHS_all_waves$meals, "1=0;2=1;3=1;4=2", as.factor = FALSE)
data_SHS_all_waves$communication <- recode(data_SHS_all_waves$communication, "1=0;2=1;3=1;4=1", as.factor = FALSE)
data_SHS_all_waves$Shopping <- recode(data_SHS_all_waves$Shopping, "1=0;2=1;3=1;4=2", as.factor = FALSE)
data_SHS_all_waves$Washing_drying <- recode(data_SHS_all_waves$Washing_drying, "1=0;2=1;3=1;4=2", as.factor = FALSE)
data_SHS_all_waves$housework_light <- recode(data_SHS_all_waves$housework_light, "1=0;2=1;3=1;4=2", as.factor = FALSE)
data_SHS_all_waves$housework_heavy <- recode(data_SHS_all_waves$housework_heavy, "1=0;2=1;3=1;4=2", as.factor = FALSE)
data_SHS_all_waves$transportation <- recode(data_SHS_all_waves$transportation, "1=0;2=1;3=1;4=1", as.factor = FALSE)
data_SHS_all_waves$Changing_body <- recode(data_SHS_all_waves$Changing_body, "1=0;2=1;3=1;4=1", as.factor = FALSE)
data_SHS_all_waves$Defecation <- recode(data_SHS_all_waves$Defecation, "0=0;1=1;2=1", as.factor = FALSE)

data_SHS_all_waves$self_care <- data_SHS_all_waves$Eating + data_SHS_all_waves$toileting

variables_Rasch <- c(
  "self_care", "washing", "Dressing", "pain", "Defecation", "Sleep", "nervous",             
  "Calm", "depressed", "Happy", "Tired", "seeing", "conversation",               
  "speak", "walk", "meals", "communication", "Changing_body",              
  "Shopping", "Washing_drying", "housework_light", "housework_heavy", "transportation"
)

data_SHS_all_waves_Rasch <- data_SHS_all_waves[, variables_Rasch]
summary(data_SHS_all_waves_Rasch)

# -----------------------------------------------------------------------------
# First Mirt Model & Anchoring
# -----------------------------------------------------------------------------
coeffs <- read.csv(
  file = here("00_Data", "item_anchor_allsample.csv"), 
  header = TRUE, 
  dec = ",", 
  sep = ";", 
  stringsAsFactors = FALSE
)

model0 <- mirt(data_SHS_all_waves_Rasch, 1, itemtype = "Rasch", pars = "values", verbose = FALSE)

for (i in 1:nrow(coeffs)) {
  for (j in 2:length(coeffs)) {
    model0[which(model0[, "item"] %in% coeffs[i, "X"] & model0[, "name"] %in% names(coeffs)[j]), "value"] <- coeffs[i, j]
  }
}

model0$est <- FALSE

model <- mirt(data_SHS_all_waves_Rasch, 1, itemtype = "Rasch", pars = model0)
DataSet2 <- extract.mirt(model, "tabdata") 
model <- mirt(DataSet2, 1, itemtype = "Rasch", pars = model0)

Thr_PCM <- coef(model, simplify = TRUE, IRTpars = TRUE)
Thr_PCM$items

LID <- residuals(model, type = "Q3")
item_fit_first <- itemfit(model, fit_stats = "infit")
person_fit_first <- personfit(model, fit_stats = "infit")

set.seed(952017)
theta_se <- fscores(model, method = "WLE", full.scores = TRUE, full.scores.SE = TRUE)
boxplot(theta_se[1, ])

# -----------------------------------------------------------------------------
# Unidimensionality & Factor Analysis
# -----------------------------------------------------------------------------
cor_poly <- polychoric(x = data_SHS_all_waves_Rasch, global = FALSE)

eigenvalues <- eigenComputes(x = data_SHS_all_waves_Rasch, use = "pairwise.complete.obs")

parallel_analysis <- eigenBootParallel(
  x = data_SHS_all_waves_Rasch, quantile = 0.95, nboot = 30, 
  option = "permutation", cor = TRUE, model = "components", 
  use = "pairwise.complete.obs"
)$quantile

results <- nScree(x = eigenvalues, aparallel = parallel_analysis)

fa_bifactor <- fa(cor_poly$rho, 4, rotate = "bifactor")
fa_bifactor1 <- fa(cor_poly$rho, 1, rotate = "bifactor")

# -----------------------------------------------------------------------------
# Differential Item Functioning (DIF) Analysis
# -----------------------------------------------------------------------------
data_SHS_all_waves$age_quest_cat <- cut(data_SHS_all_waves$ALTER, breaks = c(-Inf, 50, +Inf), labels = c(0, 1))

sci_sex_dif <- lordif(
  resp.data = data_SHS_all_waves_Rasch[, which(names(data_SHS_all_waves_Rasch) %in% c(variables_Rasch))], 
  group = data_SHS_all_waves$SEX, criterion = "Beta", beta.change = 0.1, MonteCarlo = FALSE
)

sci_age_dif <- lordif(
  resp.data = data_SHS_all_waves_Rasch[, which(names(data_SHS_all_waves_Rasch) %in% c(variables_Rasch))], 
  group = data_SHS_all_waves$age_quest_cat, criterion = "Beta", beta.change = 0.1, MonteCarlo = FALSE
)

sci_wave_dif <- lordif(
  resp.data = data_SHS_all_waves_Rasch[, which(names(data_SHS_all_waves_Rasch) %in% c(variables_Rasch))], 
  group = data_SHS_all_waves$wave, criterion = "Beta", beta.change = 0.1, MonteCarlo = FALSE
)

# -----------------------------------------------------------------------------
# Second Model with Refined Variable Set
# -----------------------------------------------------------------------------
variables_Rasch_second <- c(
  "self_care", "washing", "Defecation", "Sleep",                     
  "depressed", "speak", "Tired", 
  "seeing", "walk", "meals", "communication", "Changing_body",              
  "Shopping", "Washing_drying", "housework_light", "transportation"
)

model0 <- mirt(data_SHS_all_waves_Rasch[, which(names(data_SHS_all_waves_Rasch) %in% c(variables_Rasch_second))], 1, itemtype = "Rasch", pars = "values", verbose = FALSE)

for (i in 1:nrow(coeffs)) {
  for (j in 2:length(coeffs)) {
    model0[which(model0[, "item"] %in% coeffs[i, "X"] & model0[, "name"] %in% names(coeffs)[j]), "value"] <- coeffs[i, j]
  }
}

model0$est <- FALSE

model_anchored_second <- mirt(data_SHS_all_waves_Rasch[, which(names(data_SHS_all_waves_Rasch) %in% c(variables_Rasch_second))], 1, itemtype = "Rasch", pars = model0)
DataSet2_second <- extract.mirt(model_anchored_second, "tabdata") 
model_anchored_second <- mirt(DataSet2_second, 1, itemtype = "Rasch", pars = model0)

Thr_PCM_second <- coef(model_anchored_second, simplify = TRUE, IRTpars = TRUE)$items
item_fit_second <- itemfit(model_anchored_second, fit_stats = "infit")
LID_second <- mirt::residuals(model_anchored_second, type = "Q3")

IRT_parms_SHS <- coef(model_anchored_second, simplify = TRUE)$items
IRT_parms_SHS_fit <- coef(model_anchored_second, simplify = TRUE, IRTpars = TRUE)$items

set.seed(952017)
theta_se <- fscores(model_anchored_second, method = "WLE", full.scores = TRUE, full.scores.SE = TRUE)

# -----------------------------------------------------------------------------
# Test Information and Standard Error Curves
# -----------------------------------------------------------------------------
theta <- matrix(seq(-4, 4, .1))
info <- testinfo(model_anchored_second, Theta = theta)

df_info <- data.frame(
  theta = theta,
  Information = info,
  SE = 1 / sqrt(info)
)

scale_factor <- max(df_info$Information) / max(df_info$SE)

p_shs_info <- ggplot(df_info, aes(x = theta)) +
  geom_line(aes(y = Information, linetype = "Test Information"), linewidth = 1) +
  geom_line(aes(y = SE * scale_factor, linetype = "Standard Error"), linewidth = 1) +
  scale_linetype_manual(values = c("Test Information" = "solid", "Standard Error" = "dotted")) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(
    title = "Swiss Health Survey",
    x = "Functioning Measure (logits)",
    y = "Information",
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

p_shs_info

summary(theta_se[1, ])
empirical_rxx(theta_se)

Q3 <- mirt::residuals(model_anchored_second, type = "Q3")
test <- fa.parallel(Q3, fa = "pc")

efa <- fa(
  data_SHS_all_waves_Rasch[, which(names(data_SHS_all_waves_Rasch) %in% c(variables_Rasch_second))],
  nfactors = 3,
  rotate = "oblimin",
  fm = "ml"
)

efa$Phi

# -----------------------------------------------------------------------------
# Export Output
# -----------------------------------------------------------------------------
write.csv2(IRT_parms_SHS, file = here("00_Data", "IRT_parms_SHS.csv"), row.names = TRUE)
write.csv2(IRT_parms_SHS_fit, file = here("00_Data", "IRT_parms_SHS_fit.csv"), row.names = TRUE)