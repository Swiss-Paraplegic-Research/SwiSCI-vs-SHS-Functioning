# ==============================================================================
# Title: Data Harmonization and Preparation for Rasch Analysis (SHS Waves)
# Description: Processes and merges SHS/SwiSCI data across 2012, 2017, and 2022
# ==============================================================================

library(tidyverse)
library(gtools)
library(here)

# ==============================================================================
# 1. READ DATA BY WAVE
# ==============================================================================

# --- 2012 ---
indic12_CH <- read_delim(here("00_Data/SHS/2012/Data/indic12_CH.txt"), delim = "\t", col_names = TRUE)
sfb12_CH    <- read_delim(here("00_Data/SHS/2012/Data/sfb12_CH.txt"), delim = "\t", col_names = TRUE)
tel12_ch    <- read_delim(here("00_Data/SHS/2012/Data/tel12_ch.txt"), delim = "\t", col_names = TRUE)

# --- 2017 ---
indic17_CH <- read_delim(here("00_Data/SHS/2017/Data/indic17_CH.txt"), delim = "\t", col_names = TRUE)
sfb17_CH    <- read_delim(here("00_Data/SHS/2017/Data/sfb17_CH.txt"), delim = "\t", col_names = TRUE)
tel17_ch    <- read_delim(here("00_Data/SHS/2017/Data/tel17_ch.txt"), delim = "\t", col_names = TRUE)

# --- 2022 ---
indic22_CH <- read_delim(here("00_Data/SHS/2022/Data/indic22_CH.txt"), delim = "\t", col_names = TRUE)
sfb22_CH    <- read_delim(here("00_Data/SHS/2022/Data/sfb22_CH.txt"), delim = "\t", col_names = TRUE)
tel22_ch    <- read_delim(here("00_Data/SHS/2022/Data/tel22_ch.txt"), delim = "\t", col_names = TRUE)


# ==============================================================================
# 2. SELECT FUNCTIONING VARIABLES
# ==============================================================================

variables_functioning_survey <- c("SPSYG07d", "SPSYG07b")

variables_functioning_tel_2012 <- c(
  "TSBHD03", "THBHD02", "TRBHD03", "TBBHD03", "TKBHD02", "TIADL02a", 
  "TIADL02b", "TBADL02b", "TBADL02e", "TBADL02d", "TBADL02c", "TBADL02a", 
  "TIADL02c", "TIADL02d", "TIADL02e", "TIADL02f", "TIADL02h", "TIADL02hh", 
  "TPSYG21", "TPSYG17", "TPSYG19", "TPSYG23", "TKRSY01", "TKRSY04", "TKRSY05"
)

variables_functioning_tel_2017 <- c(
  "TSBHD03", "THBHD02", "TRBHD03", "TBBHD03", "TKBHD01", "TKBHD02", 
  "TIADL02a", "TIADL02b", "TBADL02b", "TBADL02e", "TBADL02d", "TBADL02c", 
  "TBADL02a", "TIADL02c", "TIADL02d", "TIADL02e", "TIADL02f", "TIADL02h", 
  "TIADL02hh", "TPSYG14", "TPSYG11", "TPSYG13", "TPSYG15", "TKRSY01", 
  "TKRSY04", "TKRSY05"
)

variables_functioning_tel_2022 <- c(
  "TSBHD10", "THBHD02", "TRBHD03", "TBBHD03", "TKBHD01", "TKBHD02", 
  "TIADL03a", "TIADL03b", "TBADL03b", "TBADL03e", "TBADL03d", "TBADL03c", 
  "TBADL03a", "TIADL03c", "TIADL03d", "TIADL03e", "TIADL03f", "TIADL03h", 
  "TIADL03hh", "TPSYG14", "TPSYG11", "TPSYG13", "TPSYG15", "TKRSY01", 
  "TKRSY04", "TKRSY05"
)

data_variables_functioning_tel_2012 <- cbind(tel12_ch[, "IDNO"], tel12_ch[, which(names(tel12_ch) %in% variables_functioning_tel_2012)])
data_variables_functioning_tel_2017 <- cbind(tel17_ch[, "IDNO"], tel17_ch[, which(names(tel17_ch) %in% variables_functioning_tel_2017)])
data_variables_functioning_tel_2022 <- cbind(tel22_ch[, "IDNO"], tel22_ch[, which(names(tel22_ch) %in% variables_functioning_tel_2022)])


# ==============================================================================
# 3. STANDARDIZE VARIABLE NAMES (TELEPHONE DATA)
# ==============================================================================

# Seeing
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TSBHD03")] <- "seeing"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TSBHD03")] <- "seeing"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TSBHD10")] <- "seeing"

# Conversation
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "THBHD02")] <- "conversation"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "THBHD02")] <- "conversation"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "THBHD02")] <- "conversation"

# Speak
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TRBHD03")] <- "speak"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TRBHD03")] <- "speak"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TRBHD03")] <- "speak"

# Walk
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TBBHD03")] <- "walk"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TBBHD03")] <- "walk"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TBBHD03")] <- "walk"

# Attention
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TKBHD01")] <- "attention"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TKBHD01")] <- "attention"
data_variables_functioning_tel_2012$attention <- NA

# Memory
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TKBHD02")] <- "memory"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TKBHD02")] <- "memory"
data_variables_functioning_tel_2012$memory <- NA

# Meals
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TIADL02a")] <- "meals"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TIADL02a")] <- "meals"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TIADL03a")] <- "meals"

# Communication
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TIADL02b")] <- "communication"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TIADL02b")] <- "communication"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TIADL03b")] <- "communication"

# Changing body
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TBADL02b")] <- "Changing_body"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TBADL02b")] <- "Changing_body"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TBADL03b")] <- "Changing_body"

# Washing
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TBADL02e")] <- "washing"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TBADL02e")] <- "washing"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TBADL03e")] <- "washing"

# Toileting
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TBADL02d")] <- "toileting"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TBADL02d")] <- "toileting"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TBADL03d")] <- "toileting"

# Dressing
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TBADL02c")] <- "Dressing"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TBADL02c")] <- "Dressing"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TBADL03c")] <- "Dressing"

# Eating
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TBADL02a")] <- "Eating"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TBADL02a")] <- "Eating"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TBADL03a")] <- "Eating"

# Shopping
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TIADL02c")] <- "Shopping"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TIADL02c")] <- "Shopping"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TIADL03c")] <- "Shopping"

# Washing drying
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TIADL02d")] <- "Washing_drying"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TIADL02d")] <- "Washing_drying"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TIADL03d")] <- "Washing_drying"

# Housework light
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TIADL02e")] <- "housework_light"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TIADL02e")] <- "housework_light"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TIADL03e")] <- "housework_light"

# Housework heavy
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TIADL02f")] <- "housework_heavy"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TIADL02f")] <- "housework_heavy"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TIADL03f")] <- "housework_heavy"

# Depressed
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TPSYG21")] <- "depressed"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TPSYG14")] <- "depressed"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TPSYG14")] <- "depressed"

# Nervous
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TPSYG17")] <- "nervous"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TPSYG11")] <- "nervous"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TPSYG11")] <- "nervous"

# Calm
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TPSYG19")] <- "Calm"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TPSYG13")] <- "Calm"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TPSYG13")] <- "Calm"

# Happy
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TPSYG23")] <- "Happy"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TPSYG15")] <- "Happy"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TPSYG15")] <- "Happy"

# Pain
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TKRSY01")] <- "pain"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TKRSY01")] <- "pain"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TKRSY01")] <- "pain"

# Defecation
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TKRSY04")] <- "Defecation"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TKRSY04")] <- "Defecation"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TKRSY04")] <- "Defecation"

# Sleep
colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) == "TKRSY05")] <- "Sleep"
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) == "TKRSY05")] <- "Sleep"
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) == "TKRSY05")] <- "Sleep"


# ==============================================================================
# 4. MATCH DATA ACROSS WAVES
# ==============================================================================

data_variables_functioning_tel_2012$wave <- 1
data_variables_functioning_tel_2017$wave <- 2
data_variables_functioning_tel_2022$wave <- 3

colnames(data_variables_functioning_tel_2012)[which(colnames(data_variables_functioning_tel_2012) %in% colnames(data_variables_functioning_tel_2012[,-1]))] <- paste(names(data_variables_functioning_tel_2012[,-1]), "_2012", sep="")
colnames(data_variables_functioning_tel_2017)[which(colnames(data_variables_functioning_tel_2017) %in% colnames(data_variables_functioning_tel_2017[,-1]))] <- paste(names(data_variables_functioning_tel_2017[,-1]), "_2017", sep="")
colnames(data_variables_functioning_tel_2022)[which(colnames(data_variables_functioning_tel_2022) %in% colnames(data_variables_functioning_tel_2022[,-1]))] <- paste(names(data_variables_functioning_tel_2022[,-1]), "_2022", sep="")


# ==============================================================================
# 5. SELECT AND STANDARDIZE SURVEY VARIABLES
# ==============================================================================

variables_functioning_survey_2012 <- c("TPSYG24", "TPSYG20")

data_variables_functioning_survey_2012 <- cbind(tel12_ch[, "IDNO"], tel12_ch[, which(names(tel12_ch) %in% variables_functioning_survey_2012)])
data_variables_functioning_survey_2017 <- cbind(sfb17_CH[, "IDNO"], sfb17_CH[, which(names(sfb17_CH) %in% variables_functioning_survey)])
data_variables_functioning_survey_2022 <- cbind(sfb22_CH[, "IDNO"], sfb22_CH[, which(names(sfb22_CH) %in% variables_functioning_survey)])

# Tired
colnames(data_variables_functioning_survey_2012)[which(colnames(data_variables_functioning_survey_2012) == "TPSYG24")] <- "Tired"
colnames(data_variables_functioning_survey_2017)[which(colnames(data_variables_functioning_survey_2017) == "SPSYG07d")] <- "Tired"
colnames(data_variables_functioning_survey_2022)[which(colnames(data_variables_functioning_survey_2022) == "SPSYG07d")] <- "Tired"

# Full Energy
colnames(data_variables_functioning_survey_2012)[which(colnames(data_variables_functioning_survey_2012) == "TPSYG20")] <- "Full_Energy"
colnames(data_variables_functioning_survey_2017)[which(colnames(data_variables_functioning_survey_2017) == "SPSYG07b")] <- "Full_Energy"
colnames(data_variables_functioning_survey_2022)[which(colnames(data_variables_functioning_survey_2022) == "SPSYG07b")] <- "Full_Energy"

data_variables_functioning_survey_2012$wave <- 1
data_variables_functioning_survey_2017$wave <- 2
data_variables_functioning_survey_2022$wave <- 3

colnames(data_variables_functioning_survey_2012)[which(colnames(data_variables_functioning_survey_2012) %in% colnames(data_variables_functioning_survey_2012[,-1]))] <- paste(names(data_variables_functioning_survey_2012[,-1]), "_2012", sep="")
colnames(data_variables_functioning_survey_2017)[which(colnames(data_variables_functioning_survey_2017) %in% colnames(data_variables_functioning_survey_2017[,-1]))] <- paste(names(data_variables_functioning_survey_2017[,-1]), "_2017", sep="")
colnames(data_variables_functioning_survey_2022)[which(colnames(data_variables_functioning_survey_2022) %in% colnames(data_variables_functioning_survey_2022[,-1]))] <- paste(names(data_variables_functioning_survey_2022[,-1]), "_2022", sep="")


# ==============================================================================
# 6. HEALTH CONDITIONS & TYPES
# ==============================================================================

health_conditions_variables_2012 <- c("SKRAN14b", "SKRAN15b","SKRAN16b","SKRAN17b","SKRAN18b","SKRAN19b","SKRAN20b",
                                      "SKRAN21b","SKRAN22b", "SKRAN23b","SKRAN24b", "SKRAN25b","SKRAN26b",
                                      "SKRAN27b","SKRAN28b")

health_conditions_variables_2017 <- c("TKRAN10a","TKRAN10b","TKRAN10c","TKRAN10d","TKRAN10e","TKRAN10f",
                                      "TKRAN10g","TKRAN10h","TKRAN10i","TKRAN10j")

health_conditions_variables_2022 <- c("TKRAN10l", "TKRAN10m","TKRAN10d", "TKRAN10a", "TKRAN10e","KRAN10b",
                                      "TKRAN10i", "TKRAN10o","TKRAN10j")

data_conditions_2012 <- sfb12_CH[, which(names(sfb12_CH) %in% health_conditions_variables_2012)]
sfb12_CH$health_condition <- rowSums(data_conditions_2012, na.rm = TRUE)
data_health_condition_2012 <- sfb12_CH[, c("health_condition", "IDNO")]

data_conditions_2017 <- tel17_ch[, which(names(tel17_ch) %in% health_conditions_variables_2017)]
data_conditions_2017[data_conditions_2017 == -2] <- NA
data_conditions_2017[data_conditions_2017 == -1] <- NA
data_conditions_2017[data_conditions_2017 ==  1] <- 0
data_conditions_2017[data_conditions_2017 ==  2] <- 1
tel17_ch$health_condition <- rowSums(data_conditions_2017, na.rm = TRUE)

data_conditions_2022 <- tel22_ch[, which(names(tel22_ch) %in% health_conditions_variables_2022)]
data_conditions_2022[data_conditions_2022 == -2] <- NA
data_conditions_2022[data_conditions_2022 == -1] <- NA
data_conditions_2022[data_conditions_2022 ==  1] <- 0
data_conditions_2022[data_conditions_2022 ==  2] <- 1
data_conditions_2022[data_conditions_2022 == -3] <- NA
tel22_ch$health_condition <- rowSums(data_conditions_2022, na.rm = TRUE)


# Health condition types
health_conditions_type_2012 <- c("TSUBG04", "TKRAN07", "TKRAN08", "TKRAN06")
data_conditions_type2012 <- tel12_ch[, which(names(tel12_ch) %in% health_conditions_type_2012)]
data_conditions_type2012[data_conditions_type2012 %in% c(-1, -2, -3)] <- NA
data_conditions_type2012 <- cbind(tel12_ch$IDNO, data_conditions_type2012)
data_conditions_type2012$wave <- 1
colnames(data_conditions_type2012) <- c("IDNO", "health_status", "Cronic_condition", "Activity_limitation", "Health_problem", "wave")

health_conditions_type_2017 <- c("TSUBG05", "TKRAN07", "TKRAN08", "TKRAN09")
data_conditions_type2017 <- tel17_ch[, which(names(tel17_ch) %in% health_conditions_type_2017)]
data_conditions_type2017[data_conditions_type2017 %in% c(-1, -2, -3)] <- NA
data_conditions_type2017 <- cbind(tel17_ch$IDNO, data_conditions_type2017)
data_conditions_type2017$wave <- 2
colnames(data_conditions_type2017) <- c("IDNO", "Cronic_condition", "Activity_limitation", "Health_problem", "health_status", "wave")

health_conditions_type_2022 <- c("TSUBG06", "TKRAN07", "TKRAN08", "TKRAN09")
data_conditions_type2022 <- tel22_ch[, which(names(tel22_ch) %in% health_conditions_type_2022)]
data_conditions_type2022[data_conditions_type2022 %in% c(-1, -2, -3)] <- NA
data_conditions_type2022 <- cbind(tel22_ch$IDNO, data_conditions_type2022)
data_conditions_type2022$wave <- 3
colnames(data_conditions_type2022) <- c("IDNO", "health_status", "Cronic_condition", "Activity_limitation", "Health_problem", "wave")

data_conditions_type <- smartbind(data_conditions_type2012, data_conditions_type2017, data_conditions_type2022)
write.csv2(data_conditions_type, file = here("03_Rasch/SHS/data_conditions_type.csv"), row.names = FALSE)


# ==============================================================================
# 7. FILTER VARIABLES & MERGING FUNCTIONING DATA
# ==============================================================================

variables_demographic_tel <- c("SEX", "ALTER")
variables_filter_independence_2012_2017 <- c("TKRAN08", "TSBHD03", "THBHD02", "TRBHD03", "TBBHD03")
variables_filter_independence_2022 <- c("TKRAN08", "TSBHD08", "TSBHD10", "THBHD07", "THBHD02", "TRBHD03", "TBBHD03")

data_tel_2012_filter <- tel12_ch[, which(names(tel12_ch) %in% c(variables_demographic_tel, variables_filter_independence_2012_2017))]
data_tel_2017_filter <- tel17_ch[, which(names(tel17_ch) %in% c(variables_demographic_tel, variables_filter_independence_2012_2017))]
data_tel_2022_filter <- tel22_ch[, which(names(tel22_ch) %in% c(variables_demographic_tel, variables_filter_independence_2022))]

colnames(data_tel_2012_filter) <- paste(names(data_tel_2012_filter), "_2012", sep="")
colnames(data_tel_2017_filter) <- paste(names(data_tel_2017_filter), "_2017", sep="")  
colnames(data_tel_2022_filter) <- paste(names(data_tel_2022_filter), "_2022", sep="")  

data_functioning_tel_filter_2012 <- cbind(data_variables_functioning_tel_2012, data_tel_2012_filter)

data_functioning_tel_filter_2017 <- cbind(data_variables_functioning_tel_2017, data_tel_2017_filter, tel17_ch$health_condition)
colnames(data_functioning_tel_filter_2017)[ncol(data_functioning_tel_filter_2017)] <- "health_condition"

data_functioning_tel_filter_2022 <- cbind(data_variables_functioning_tel_2022, data_tel_2022_filter, tel22_ch$health_condition)
colnames(data_functioning_tel_filter_2022)[ncol(data_functioning_tel_filter_2022)] <- "health_condition"

data_functioning_survey_filter_2012 <- data_variables_functioning_survey_2012
data_functioning_survey_filter_2017 <- cbind(data_variables_functioning_survey_2017, sfb17_CH$SSEX, sfb17_CH$SALTER)
data_functioning_survey_filter_2022 <- cbind(data_variables_functioning_survey_2022, sfb22_CH$SSEX, sfb22_CH$SALTER)

colnames(data_functioning_survey_filter_2017)[3:4] <- c("SSEX_2017", "SALTER_2017")
colnames(data_functioning_survey_filter_2022)[3:4] <- c("SSEX_2022", "SALTER_2022")

functioning_data_2012 <- merge(data_functioning_tel_filter_2012, data_functioning_survey_filter_2012, by = "IDNO", all = TRUE)
functioning_data_2012 <- merge(functioning_data_2012, data_health_condition_2012, by = "IDNO", all = TRUE)

functioning_data_2017 <- merge(data_functioning_tel_filter_2017, data_functioning_survey_filter_2017, by = "IDNO", all = TRUE)
functioning_data_2022 <- merge(data_functioning_tel_filter_2022, data_functioning_survey_filter_2022, by = "IDNO", all = TRUE)

write.csv2(functioning_data_2012, file = here("03_Rasch/SHS/functioning_data_2012.csv"), row.names = FALSE)
write.csv2(functioning_data_2017, file = here("03_Rasch/SHS/functioning_data_2017.csv"), row.names = FALSE)
write.csv2(functioning_data_2022, file = here("03_Rasch/SHS/functioning_data_2022.csv"), row.names = FALSE)


# ==============================================================================
# 8. TABLES AND OUTPUT CAPTURE
# ==============================================================================

table_2012 <- lapply(functioning_data_2012[,-1], table)
table_2017 <- lapply(functioning_data_2017[,-1], table)
table_2022 <- lapply(functioning_data_2022[,-1], table)

capture.output(table_2012, file = here("03_Rasch/SHS/table_2012.txt"))
capture.output(table_2017, file = here("03_Rasch/SHS/table_2017.txt"))
capture.output(table_2022, file = here("03_Rasch/SHS/table_2022.txt"))


# ==============================================================================
# 9. FILTER CORRECTIONS (2012)
# ==============================================================================

var_missing_2012 <- c("Eating_2012", "Changing_body_2012", "Dressing_2012", "toileting_2012", "washing_2012", "meals_2012",
                      "communication_2012", "Shopping_2012", "Washing_drying_2012", "housework_light_2012", "housework_heavy_2012")

for (i in seq_along(var_missing_2012)) {
  v_name <- var_missing_2012[i]
  col_idx <- which(names(functioning_data_2012) == v_name)
  
  cond <- functioning_data_2012$ALTER_2012 < 65 & 
    (functioning_data_2012$TKRAN08_2012 %in% c(3, -1, -2) | is.na(functioning_data_2012$TKRAN08_2012)) &  
    (functioning_data_2012$TSBHD03_2012 %in% c(1, -2) | is.na(functioning_data_2012$TSBHD03_2012)) &
    (functioning_data_2012$THBHD02_2012 %in% c(1, -2) | is.na(functioning_data_2012$THBHD02_2012)) &  
    (functioning_data_2012$TRBHD03_2012 %in% c(1, -2) | is.na(functioning_data_2012$TRBHD03_2012)) &
    (functioning_data_2012$TBBHD03_2012 %in% c(1, -2) | is.na(functioning_data_2012$TBBHD03_2012))
  
  functioning_data_2012[cond, col_idx] <- 1
}

for (v_name in var_missing_2012) {
  col_idx <- which(names(functioning_data_2012) == v_name)
  functioning_data_2012[functioning_data_2012[, col_idx] == -3, col_idx] <- NA
}

# Transportation correction
functioning_data_2012$transportation_2012 <- functioning_data_2012$TIADL02h_2012

cond_trans <- functioning_data_2012$ALTER_2012 < 65 & 
  (functioning_data_2012$TKRAN08_2012 %in% c(3, -1, -2) | is.na(functioning_data_2012$TKRAN08_2012)) &  
  (functioning_data_2012$TSBHD03_2012 %in% c(1, -2) | is.na(functioning_data_2012$TSBHD03_2012)) &
  (functioning_data_2012$THBHD02_2012 %in% c(1, -2) | is.na(functioning_data_2012$THBHD02_2012)) &  
  (functioning_data_2012$TRBHD03_2012 %in% c(1, -2) | is.na(functioning_data_2012$TRBHD03_2012)) &
  (functioning_data_2012$TBBHD03_2012 %in% c(1, -2) | is.na(functioning_data_2012$TBBHD03_2012))

functioning_data_2012$transportation_2012[cond_trans] <- functioning_data_2012$TIADL02hh_2012[cond_trans]
functioning_data_2012$transportation_2012[functioning_data_2012$transportation_2012 == -3] <- NA
                       
# Emotional variables missing values
 emotional_variables_2012 <- c("nervous_2012", "Calm_2012", "depressed_2012", "Happy_2012", "Tired_2012", "Full_Energy_2012")
                       for (v_name in emotional_variables_2012) {
                         col_idx <- which(names(functioning_data_2012) == v_name)
                         functioning_data_2012[functioning_data_2012[, col_idx] == -6, col_idx] <- NA
                       }
                       
                       functioning_data_2012[functioning_data_2012 == -2] <- NA
                       functioning_data_2012[functioning_data_2012 == -4] <- NA
                       
 write.csv2(functioning_data_2012, file = here("03_Rasch/SHS/functioning_data_2012_analysis.csv"), row.names = FALSE)
 
 # ==============================================================================
 # 10. FILTER CORRECTIONS (2017)
 # ==============================================================================
 
 var_missing_2017 <- c("Eating_2017", "Changing_body_2017", "Dressing_2017", "toileting_2017", "washing_2017", "meals_2017",
                       "communication_2017", "Shopping_2017", "Washing_drying_2017", "housework_light_2017", "housework_heavy_2017")
 
 for (i in seq_along(var_missing_2017)) {
   v_name <- var_missing_2017[i]
   col_idx <- which(names(functioning_data_2017) == v_name)
   
cond <- functioning_data_2017$SALTER_2017 < 65 & 
     (functioning_data_2017$TKRAN08_2017 %in% c(3, -1, -2) | is.na(functioning_data_2017$TKRAN08_2017)) &  
     (functioning_data_2017$TSBHD03_2017 %in% c(1, -2) | is.na(functioning_data_2017$TSBHD03_2017)) &
     (functioning_data_2017$THBHD02_2017 %in% c(1, -2) | is.na(functioning_data_2017$THBHD02_2017)) &  
     (functioning_data_2017$TRBHD03_2017 %in% c(1, -2) | is.na(functioning_data_2017$TRBHD03_2017)) &
     (functioning_data_2017$TBBHD03_2017 %in% c(1, -2) | is.na(functioning_data_2017$TBBHD03_2017))
   
   functioning_data_2017[cond, col_idx] <- 1
 }
 
 for (v_name in var_missing_2017) {
   col_idx <- which(names(functioning_data_2017) == v_name)
   functioning_data_2017[functioning_data_2017[, col_idx] == -3, col_idx] <- NA
 }
 
 # Transportation correction (2017)
 
functioning_data_2017$transportation_2017 <- functioning_data_2017$TIADL02h_2017
 
 cond_trans_2017 <- functioning_data_2017$SALTER_2017 < 65 & 
   (functioning_data_2017$TKRAN08_2017 %in% c(3, -1, -2) | is.na(functioning_data_2017$TKRAN08_2017)) &  
   (functioning_data_2017$TSBHD03_2017 %in% c(1, -2) | is.na(functioning_data_2017$TSBHD03_2017)) &
   (functioning_data_2017$THBHD02_2017 %in% c(1, -2) | is.na(functioning_data_2017$THBHD02_2017)) &  
   (functioning_data_2017$TRBHD03_2017 %in% c(1, -2) | is.na(functioning_data_2017$TRBHD03_2017)) &
   (functioning_data_2017$TBBHD03_2017 %in% c(1, -2) | is.na(functioning_data_2017$TBBHD03_2017))
 
functioning_data_2017$transportation_2017[cond_trans_2017] <- functioning_data_2017$TIADL02hh_2017[cond_trans_2017]
 functioning_data_2017$transportation_2017[functioning_data_2017$transportation_2017 == -3] <- NA
                        
# Emotional variables missing values (2017)
emotional_variables_2017 <- c("nervous_2017", "Calm_2017", "depressed_2017", "Happy_2017", "Tired_2017", "Full_Energy_2017")
                        for (v_name in emotional_variables_2017) {
                          if (v_name %in% names(functioning_data_2017)) {
                            col_idx <- which(names(functioning_data_2017) == v_name)
                            functioning_data_2017[functioning_data_2017[, col_idx] %in% c(-6, -2, -4), col_idx] <- NA
                          }
                        }
                        
functioning_data_2017[functioning_data_2017 == -2] <- NA
functioning_data_2017[functioning_data_2017 == -4] <- NA
                        
write.csv2(functioning_data_2017, file = here("03_Rasch/SHS/functioning_data_2017_analysis.csv"), row.names = FALSE)
                        
                        
 # ==============================================================================
# 11. FILTER CORRECTIONS (2022)
 # ==============================================================================
                        
var_missing_2022 <- c("Eating_2022", "Changing_body_2022", "Dressing_2022", "toileting_2022", "washing_2022", "meals_2022",
                                              "communication_2022", "Shopping_2022", "Washing_drying_2022", "housework_light_2022", "housework_heavy_2022")
                        
for (i in seq_along(var_missing_2022)) {
                          v_name <- var_missing_2022[i]
                          col_idx <- which(names(functioning_data_2022) == v_name)
                          
                          cond <- functioning_data_2022$SALTER_2022 < 65 & 
                            (functioning_data_2022$TKRAN08_2022 %in% c(3, -1, -2) | is.na(functioning_data_2022$TKRAN08_2022)) &  
                            (functioning_data_2022$TSBHD10_2022 %in% c(1, -2) | is.na(functioning_data_2022$TSBHD10_2022)) &
                            (functioning_data_2022$THBHD02_2022 %in% c(1, -2) | is.na(functioning_data_2022$THBHD02_2022)) &  
                            (functioning_data_2022$TRBHD03_2022 %in% c(1, -2) | is.na(functioning_data_2022$TRBHD03_2022)) &
                            (functioning_data_2022$TBBHD03_2022 %in% c(1, -2) | is.na(functioning_data_2022$TBBHD03_2022))
                          
                          functioning_data_2022[cond, col_idx] <- 1
                        }
                        
for (v_name in var_missing_2022) {
                          col_idx <- which(names(functioning_data_2022) == v_name)
                          functioning_data_2022[functioning_data_2022[, col_idx] == -3, col_idx] <- NA
                        }
                        
# Transportation correction (2022)
functioning_data_2022$transportation_2022 <- functioning_data_2022$TIADL03h_2022
                        
cond_trans_2022 <- functioning_data_2022$SALTER_2022 < 65 & 
                          (functioning_data_2022$TKRAN08_2022 %in% c(3, -1, -2) | is.na(functioning_data_2022$TKRAN08_2022)) &  
                          (functioning_data_2022$TSBHD10_2022 %in% c(1, -2) | is.na(functioning_data_2022$TSBHD10_2022)) &
                          (functioning_data_2022$THBHD02_2022 %in% c(1, -2) | is.na(functioning_data_2022$THBHD02_2022)) &  
                          (functioning_data_2022$TRBHD03_2022 %in% c(1, -2) | is.na(functioning_data_2022$TRBHD03_2022)) &
                          (functioning_data_2022$TBBHD03_2022 %in% c(1, -2) | is.na(functioning_data_2022$TBBHD03_2022))
                        
functioning_data_2022$transportation_2022[cond_trans_2022] <- functioning_data_2022$TIADL03hh_2022[cond_trans_2022]
functioning_data_2022$transportation_2022[functioning_data_2022$transportation_2022 == -3] <- NA
                                               
# Emotional variables missing values (2022)
emotional_variables_2022 <- c("nervous_2022", "Calm_2022", "depressed_2022", "Happy_2022", "Tired_2022", "Full_Energy_2022")
                                               for (v_name in emotional_variables_2022) {
                                                 if (v_name %in% names(functioning_data_2022)) {
                                                   col_idx <- which(names(functioning_data_2022) == v_name)
                                                   functioning_data_2022[functioning_data_2022[, col_idx] %in% c(-6, -2, -4), col_idx] <- NA
                                                 }
                                               }
                                               
functioning_data_2022[functioning_data_2022 == -2] <- NA
functioning_data_2022[functioning_data_2022 == -4] <- NA
                                               
write.csv2(functioning_data_2022, file = here("03_Rasch/SHS/functioning_data_2022_analysis.csv"), row.names = FALSE)