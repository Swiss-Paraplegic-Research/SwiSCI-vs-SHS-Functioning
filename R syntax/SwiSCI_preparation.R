################ Title: Data Set Preparation ###########################
################ Author: Cristina Ehrmann ###########################
################ Date: 11.12.2019 ###########################

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------
library(xlsx)
library(sjlabelled)
library(plyr)
library(lubridate)
library(remotes)
library(tidyverse)
library(here)
library(gtools)

# -----------------------------------------------------------------------------
# Function Definitions
# -----------------------------------------------------------------------------
recode <- function(v, alt, neu) {
  # Test that the vectors alt and neu have the same length
  if (length(alt) != length(neu)) stop("alt and neu must have same length!")
  
  v_neu <- v
  for (j in 1:length(alt)) {
    v_neu[v == alt[j]] <- neu[j]
  }
  return(v_neu)
}

# -----------------------------------------------------------------------------
# Reading in the Original Data (Using relative paths via 'here')
# -----------------------------------------------------------------------------
path_results_data <- here("00_Data", "2024-N-002")

SwiSCI_2012 <- read.csv(file = file.path(path_results_data, "2024-N-002_2012__2025-03-25.csv"), header = TRUE, dec = ",", sep = ";", stringsAsFactors = FALSE)
SwiSCI_2017 <- read.csv2(file = file.path(path_results_data, "2024-N-002_2017__2025-03-25.csv"), header = TRUE, dec = ",", sep = ",", stringsAsFactors = FALSE)
SwiSCI_2022 <- read.csv2(file = file.path(path_results_data, "2024-N-002_2022__2025-03-25.csv"), header = TRUE, dec = ",", sep = ",", stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Select Variables Over Time
# -----------------------------------------------------------------------------
data_common_items_2012 <- SwiSCI_2012[, which(names(SwiSCI_2012) %in% c(
  "id_swisci", "sex", "age_sci", "sci_cause_type",
  "sci_type", "sci_degree", "survey_123", "ts1_marital_status", "ts1_age_quest_admin",
  "ts1_education_yrs", "ts1_time_since_sci", "ts1b_language",
  "ts1b_quest_type", "ts1_problem_pain", "ts1_problem_bowel",
  "ts1_problem_bladder", "ts1_problem_sexual", "ts1_problem_contractures",
  "ts1_problem_spasticity", "ts1_problem_pressure", "ts1_problem_sleep",
  "ts1_ap_mobility_limit", "ap_household_limit", "ts1_scim_1", "ts1_scim_2", "ts1_scim_3", "ts1_scim_4",
  "ts1_scim_5", "ts1_scim_6", "ts1_scim_14", "ts1_scim_16",
  "ts1_scim_19"
))]

colnames(data_common_items_2012) <- c(
  "id_swisci", "sex", "age_sci", "sci_cause_type",
  "sci_type", "sci_degree", "survey_123", "marital_status",
  "education_yrs", "time_since_sci", "language", "age_quest",
  "quest_type", "problem_pain", "problem_bowel",
  "problem_bladder", "problem_sexual", "problem_contractures",
  "problem_spasticity", "problem_pressure", "problem_sleep",
  "ap_mobility_limit", "scim_1", "scim_2", "scim_3", "scim_4",
  "scim_5", "scim_6", "scim_14", "scim_16",
  "scim_19"
)

data_common_items_2012$wave <- 1

data_common_items_2017 <- SwiSCI_2017[, which(names(SwiSCI_2017) %in% c(
  "id_swisci", "sex", "age_sci", "sci_cause_type",
  "sci_type", "sci_degree", "survey_123", "ts2_marital_status", "ts2_age_quest",
  "ts2_education_yrs", "ts2_time_since_sci", "ts2a_language",
  "ts2a_quest_type", "ts2_problem_pain", "ts2_problem_bowel",
  "ts2_problem_bladder", "ts2_problem_sexual", "ts2_problem_contractures",
  "ts2_problem_spasticity", "ts2_problem_pressure", "ts2_problem_sleep",
  "ts2_ap_mobility_limit", "ts2_scim_1", "ts2_scim_2", "ts2_scim_3", "ts2_scim_4",
  "ts2_scim_5", "ts2_scim_6", "ts2_scim_14", "ts2_scim_16",
  "ts2_scim_19"
))]

colnames(data_common_items_2017) <- c(
  "id_swisci", "sex", "age_sci", "sci_cause_type",
  "sci_type", "sci_degree", "survey_123", "marital_status",
  "education_yrs", "time_since_sci", "language", "age_quest",
  "quest_type", "problem_pain", "problem_bowel",
  "problem_bladder", "problem_sexual", "problem_contractures",
  "problem_spasticity", "problem_pressure", "problem_sleep",
  "ap_mobility_limit", "scim_1", "scim_2", "scim_3", "scim_4",
  "scim_5", "scim_6", "scim_14", "scim_16",
  "scim_19"
)

data_common_items_2017$wave <- 2

data_common_items_2022 <- SwiSCI_2022[, which(names(SwiSCI_2022) %in% c(
  "id_swisci", "sex", "age_sci", "sci_cause_type",
  "sci_type", "sci_degree", "survey_123", "ts3_marital_status", "ts3_age_quest",
  "ts3_education_yrs", "ts3_time_since_sci", "ts3a_language",
  "ts3a_quest_type", "ts3_problem_pain", "ts3_problem_bowel",
  "ts3_problem_bladder", "ts3_problem_sexual", "ts3_problem_contractures",
  "ts3_problem_spasticity", "ts3_problem_pressure", "ts3_problem_sleep",
  "ts3_ap_mobility_limit", "ts3_scim_1", "ts3_scim_2", "ts3_scim_3", "ts3_scim_4",
  "ts3_scim_5", "ts3_scim_6", "ts3_scim_14", "ts3_scim_16",
  "ts3_scim_19"
))]

colnames(data_common_items_2022) <- c(
  "id_swisci", "sex", "age_sci", "sci_cause_type",
  "sci_type", "sci_degree", "survey_123", "marital_status",
  "education_yrs", "time_since_sci", "language", "age_quest",
  "quest_type", "problem_pain", "problem_bowel",
  "problem_bladder", "problem_sexual", "problem_contractures",
  "problem_spasticity", "problem_pressure", "problem_sleep",
  "ap_mobility_limit", "scim_1", "scim_2", "scim_3", "scim_4",
  "scim_5", "scim_6", "scim_14", "scim_16",
  "scim_19"
)

data_common_items_2022$wave <- 3

data_common_items <- smartbind(data_common_items_2012, data_common_items_2017, data_common_items_2022)

# -----------------------------------------------------------------------------
# Add Items
# -----------------------------------------------------------------------------
# Depressed
data_common_items\(depression2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_eaf_depressed, SwiSCI_2022$ts3_eaf_depressed)
data_common_items\(depression2012 <- c(SwiSCI_2012\)ts1_mood_depressed, rep(NA, 1529), rep(NA, 1319))

# Nervous
data_common_items\(nervous2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_eaf_nervous, SwiSCI_2022$ts3_eaf_nervous)
data_common_items\(nervous2012 <- c(SwiSCI_2012\)ts1_mood_nervous, rep(NA, 1529), rep(NA, 1319))

# Calm
data_common_items\(calm2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_eaf_calm, SwiSCI_2022$ts3_eaf_calm)
data_common_items\(calm2012 <- c(SwiSCI_2012\)ts1_mood_calm, rep(NA, 1529), rep(NA, 1319))

# Happy
data_common_items\(happy2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_eaf_happy, SwiSCI_2022$ts3_eaf_happy)
data_common_items\(happy2012 <- c(SwiSCI_2012\)ts1_mood_happy, rep(NA, 1529), rep(NA, 1319))

# Tired
data_common_items\(tired2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_eaf_tired, SwiSCI_2022$ts3_eaf_tired)
data_common_items\(tired2012 <- c(SwiSCI_2012\)ts1_tiredness, rep(NA, 1529), rep(NA, 1319))

# Full of energy
data_common_items\(fulllife2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_eaf_life, SwiSCI_2022$ts3_eaf_life)

# Daily routine
data_common_items\(dailyroutine2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_routine, SwiSCI_2022$ts3_ap_routine)

# Stress
data_common_items\(stress2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_stress, SwiSCI_2022$ts3_ap_stress)

# Hands
data_common_items\(hands2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_hands, SwiSCI_2022$ts3_ap_hands)

# Getting
data_common_items\(getting2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_getting, SwiSCI_2022$ts3_ap_getting)

# Public transportation
data_common_items\(public_transportation2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_transport_pub, SwiSCI_2022$ts3_ap_transport_pub)

# Private transportation
data_common_items\(privat_transportation2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_transport_priv, SwiSCI_2022$ts3_ap_transport_priv)

# Getting up
data_common_items\(gettingup2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_getup, SwiSCI_2022$ts3_ap_getup)

# Lay down
data_common_items\(laydown2017_2022 <- c(rep(NA, 1549), SwiSCI_2017\)ts2_ap_laydown, SwiSCI_2022$ts3_ap_laydown)

# -----------------------------------------------------------------------------
# Export Output
# -----------------------------------------------------------------------------
write.csv2(
  data_common_items, 
  file = here("03_Rasch", "SwiSCI", "functioning_data_SwiSCI.csv"), 
  row.names = FALSE
)