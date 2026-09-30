################ Title: Rasch Analysis and DIF - SwiSCI ####################
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
library(here)

# -----------------------------------------------------------------------------
# Reading in Data (Using relative paths via 'here')
# -----------------------------------------------------------------------------
data_SwiSCI_all_waves <- read.csv(
  file = here("00_Data", "data_SCIM_rasch_allwaves_persons.csv"), 
  header = TRUE, 
  dec = ",", 
  sep = ";", 
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Data Recoding & Preparation
# -----------------------------------------------------------------------------
recode <- car::recode

data_SwiSCI_all_waves$problem_sexual <- recode(data_SwiSCI_all_waves$problem_sexual, "0=0;1=1;2=1;3=2", as.factor = FALSE)
data_SwiSCI_all_waves$problem_contractures <- recode(data_SwiSCI_all_waves$problem_contractures, "0=0;1=1;2=1;3=2", as.factor = FALSE)
data_SwiSCI_all_waves$problem_pressure <- recode(data_SwiSCI_all_waves$problem_pressure, "0=0;1=1;2=1;3=2", as.factor = FALSE)
data_SwiSCI_all_waves$Defecation <- recode(data_SwiSCI_all_waves$Defecation, "0=0;1=1;2=1", as.factor = FALSE)

data_SwiSCI_all_waves$self_care <- data_SwiSCI_all_waves$Eating + data_SwiSCI_all_waves$toileting

variables_Rasch <- c(
  "problem_bladder", "problem_sexual", "problem_contractures", 
  "problem_spasticity", "problem_pressure", "ap_mobility_limit",        
  "scim_6", "scim_16", "scim_19",                        
  "self_care", "pain", "Defecation", "Sleep", "nervous",                        
  "Calm", "depressed", "Happy", "Tired"
)

data_SwiSCI_all_waves_Rasch <- data_SwiSCI_all_waves[, variables_Rasch]

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

model0 <- mirt(data_SwiSCI_all_waves_Rasch[, variables_Rasch], 1, itemtype = "Rasch", pars = "values", verbose = FALSE)

for (i in 1:nrow(coeffs)) {
  for (j in 2:length(coeffs)) {
    model0[which(model0[, "item"] %in% coeffs[i, "X"] & model0[, "name"] %in% names(coeffs)[j]), "value"] <- coeffs[i, j]
  }
}

model0$est <- FALSE

model_anchored <- mirt(data_SwiSCI_all_waves_Rasch, 1, itemtype = "Rasch", pars = model0)
DataSet2 <- extract.mirt(model_anchored, "tabdata") 
model <- mirt(DataSet2, 1, itemtype = "Rasch", pars = model0)

Thr_PCM <- coef(model, simplify = TRUE, IRTpars = TRUE)$items
LID <- mirt::residuals(model, type = "Q3")
item_fit_first <- itemfit(model, fit_stats = "infit")

# -----------------------------------------------------------------------------
# Unidimensionality & Factor Analysis
# -----------------------------------------------------------------------------
cor_poly <- polychoric(x = data_SwiSCI_all_waves_Rasch, global = FALSE)

eigenvalues <- eigenComputes(x = data_SwiSCI_all_waves_Rasch, use = "pairwise.complete.obs")

parallel_analysis <- eigenBootParallel(
  x = data_SwiSCI_all_waves_Rasch, quantile = 0.95, nboot = 30, 
  option = "permutation", cor = TRUE, model = "components", 
  use = "pairwise.complete.obs"
)$quantile

results <- nScree(x = eigenvalues, aparallel = parallel_analysis)

fa_bifactor <- fa(cor_poly$rho, 3, rotate = "bifactor")
fa_bifactor1 <- fa(cor_poly$rho, 1, rotate = "bifactor")

# -----------------------------------------------------------------------------
# Testlet Creation & Second Model
# -----------------------------------------------------------------------------
data_SwiSCI_all_waves_Rasch$self_care2 <- data_SwiSCI_all_waves_Rasch$scim_6 +
  data_SwiSCI_all_waves_Rasch$scim_16 +
  data_SwiSCI_all_waves_Rasch$scim_19

variables_Rasch_second <- c(
  "problem_bladder", "problem_sexual", "problem_contractures",
  "problem_spasticity", "problem_pressure", "ap_mobility_limit",    
  "self_care2", "self_care", "pain", "Sleep", "nervous",                    
  "Calm", "depressed", "Happy"
)

model0 <- mirt(data_SwiSCI_all_waves_Rasch[, variables_Rasch_second], 1, itemtype = "Rasch", pars = "values")

for (i in 1:nrow(coeffs)) {
  for (j in 2:length(coeffs)) {
    model0[which(model0[, "item"] %in% coeffs[i, "X"] & model0[, "name"] %in% names(coeffs)[j]), "value"] <- coeffs[i, j]
  }
}

model0$est <- FALSE

model_anchored_second <- mirt(data_SwiSCI_all_waves_Rasch[, variables_Rasch_second], 1, itemtype = "Rasch", pars = model0)
DataSet2_second <- extract.mirt(model_anchored_second, "tabdata") 
model_anchored_second <- mirt(DataSet2_second, 1, itemtype = "Rasch", pars = model0)

Thr_PCM_second <- coef(model_anchored_second, simplify = TRUE, IRTpars = TRUE)$items

Sample_Size <- nrow(DataSet2_second)
Cut_Infit <- 1 + (2 / sqrt(Sample_Size))
Cut_Outfit <- 1 + (6 / sqrt(Sample_Size))

item_fit_second <- itemfit(model_anchored_second, fit_stats = "infit")
LID_second <- mirt::residuals(model_anchored_second, type = "Q3")

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

p_swisci_info <- ggplot(df_info, aes(x = theta)) +
  geom_line(aes(y = Information, linetype = "Test Information"), linewidth = 1) +
  geom_line(aes(y = SE * scale_factor, linetype = "Standard Error"), linewidth = 1) +
  scale_linetype_manual(values = c("Test Information" = "solid", "Standard Error" = "dotted")) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(
    title = "SwiSCI Community Survey",
    x = "Functioning Measure (logits)",
    y = "Information",
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

IRT_parms_SwiSCI <- coef(model_anchored_second, simplify = TRUE)$items
IRT_parms_SwiSCI_fit <- coef(model_anchored_second, simplify = TRUE, IRTpars = TRUE)$items

write.csv2(IRT_parms_SwiSCI, file = here("00_Data", "IRT_parms_SwiSCI.csv"), row.names = TRUE)
write.csv2(IRT_parms_SwiSCI_fit, file = here("00_Data", "IRT_parms_SwiSCI_fit.csv"), row.names = TRUE)

# -----------------------------------------------------------------------------
# Differential Item Functioning (DIF) Analysis
# -----------------------------------------------------------------------------
data_SwiSCI_all_wave$age_quest_cat <- cut(data_SwiSCI_all_waves$age_quest, breaks = c(-Inf, 53, +Inf), labels = c(0, 1))

sci_sex_dif <- lordif(
  resp.data = data_SwiSCI_all_waves_Rasch[, variables_Rasch_second], 
  group = data_SwiSCI_all_waves$sex, criterion = "Beta", beta.change = 0.1, MonteCarlo = FALSE
)

sci_age_dif <- lordif(
  resp.data = data_SwiSCI_all_waves_Rasch[, variables_Rasch_second], 
  group = data_SwiSCI_all_waves$age_quest_cat, criterion = "Beta", beta.change = 0.1, MonteCarlo = FALSE, anchor = data_SwiSCI_all_waves_Rasch$wave
)

sci_wave_dif <- lordif(
  resp.data = data_SwiSCI_all_waves_Rasch[, variables_Rasch_second], 
  group = data_SwiSCI_all_waves$wave, criterion = "Beta", beta.change = 0.1, MonteCarlo = FALSE
)

# -----------------------------------------------------------------------------
# Reliability and Final Summary
# -----------------------------------------------------------------------------
set.seed(952017)

theta_se <- fscores(model_anchored_second, method = "WLE", full.scores = TRUE, full.scores.SE = TRUE)
empirical_rxx(theta_se)

summary_parameters <- fscores(model, method = "WLE", full.scores = FALSE, full.scores.SE = TRUE)
Q3 <- residuals(model_anchored_second, type = "Q3")