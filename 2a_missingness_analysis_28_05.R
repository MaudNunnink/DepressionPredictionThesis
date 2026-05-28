

# ============================================================
# 02a_missingness_analysis.R
# Missingness analysis for key covariates
# ============================================================

library(tidyverse)
library(haven)

# Load constructed panel dataset -----------------------------------------

panel_full <- readRDS("panel_full.rds")


# Rename and prepare variables needed for missingness analysis ------------

missing_analysis_data <- panel_full %>%
  rename(
    educational_level = isced1997_r,
    employment_status = ep005_,
    marital_status = dn014_,
    smoke_present = br002_
  ) %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels)) %>%
  mutate(
    # Outcome
    eurodcat_w6 = factor(
      as.character(eurodcat_w6),
      levels = c("0", "1"),
      labels = c("no", "yes")
    ),
    
    # Gender
    gender = factor(gender),
    
    # Chronic condition count
    chronicw5 = na_if(chronicw5, -1),
    chronicw5 = na_if(chronicw5, -2),
    
    chronic_cat = case_when(
      chronicw5 == 0 ~ "0",
      chronicw5 == 1 ~ "1",
      chronicw5 == 2 ~ "2",
      chronicw5 == 3 ~ "3",
      chronicw5 >= 4 ~ "4+",
      TRUE ~ NA_character_
    ),
    
    chronic_cat = factor(
      chronic_cat,
      levels = c("0", "1", "2", "3", "4+")
    ),
    
    # Education
    educational_level = na_if(educational_level, 97),
    
    education_3cat = case_when(
      educational_level %in% c(0, 1, 2) ~ "low",
      educational_level %in% c(3, 4)    ~ "mid",
      educational_level %in% c(5, 6)    ~ "high",
      TRUE ~ NA_character_
    ),
    
    education_3cat = factor(
      education_3cat,
      levels = c("low", "mid", "high")
    ),
    
    # Employment
    employment_status = as.numeric(as.character(employment_status)),
    employment_status = na_if(employment_status, 97),
    
    employment_3cat = case_when(
      employment_status == 2 ~ "employed",
      employment_status == 1 ~ "retired",
      employment_status %in% c(3, 4, 5) ~ "not_working",
      TRUE ~ NA_character_
    ),
    
    employment_3cat = factor(
      employment_3cat,
      levels = c("employed", "retired", "not_working")
    ),
    
    # Marital status
    marital_3cat = case_when(
      marital_status %in% c(1, 2, 3) ~ "married",
      marital_status %in% c(4, 5)    ~ "not_married",
      marital_status == 6            ~ "widowed",
      TRUE                           ~ "missing"
    ),
    
    marital_3cat = factor(
      marital_3cat,
      levels = c("married", "not_married", "widowed", "missing")
    ),
    
    # Smoking
    smoke_3cat = case_when(
      smoke_present == 1   ~ "smoking",
      smoke_present == 5   ~ "non_smoking",
      is.na(smoke_present) ~ "unknown",
      TRUE ~ NA_character_
    ),
    
    smoke_3cat = factor(
      smoke_3cat,
      levels = c("non_smoking", "smoking", "unknown")
    )
  ) %>%
  mutate(
    missing_smoking = ifelse(smoke_3cat == "unknown", 1, 0),
    missing_marital = ifelse(marital_3cat == "missing", 1, 0),
    missing_employment = ifelse(is.na(employment_3cat), 1, 0),
    missing_education = ifelse(is.na(education_3cat), 1, 0)
  )


# ============================================================
# Smoking status missingness
# ============================================================

missing_smoking_model <- glm(
  missing_smoking ~ age2013 +
    gender +
    education_3cat +
    chronic_cat +
    eurodcat_w6,
  
  data = missing_analysis_data,
  family = binomial
)

summary(missing_smoking_model)


# ============================================================
# Marital status missingness
# ============================================================

missing_marital_model <- glm(
  missing_marital ~ age2013 +
    gender +
    education_3cat +
    chronic_cat +
    eurodcat_w6,
  
  data = missing_analysis_data,
  family = binomial
)

summary(missing_marital_model)


# ============================================================
# Employment status missingness
# ============================================================

missing_employment_model <- glm(
  missing_employment ~ age2013 +
    gender +
    education_3cat +
    chronic_cat +
    eurodcat_w6,
  
  data = missing_analysis_data,
  family = binomial
)

summary(missing_employment_model)


# ============================================================
# Education missingness
# Education not included as predictor because education missingness is the outcome
# ============================================================

missing_education_model <- glm(
  missing_education ~ age2013 +
    gender +
    chronic_cat +
    eurodcat_w6,
  
  data = missing_analysis_data,
  family = binomial
)

summary(missing_education_model)