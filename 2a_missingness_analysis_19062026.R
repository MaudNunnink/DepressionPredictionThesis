

# ============================================================
# 02a_missingness_analysis.R
# Missingness analysis for key covariates
# ============================================================

library(tidyverse)
library(haven)
library(broom)

# Load constructed panel dataset -----------------------------------------

panel_full <- readRDS("panel_full.rds")


# ============================================================
# Variable names to check
# ============================================================
# Adjust these names if they differ in your dataset.
# If you are not sure, run: names(panel_full)

alcohol_var     <- "alcohol_w5"
moderate_pa_var <- "moderate_pa_w5"
vigorous_pa_var <- "vigorous_pa_w5"


# Helper function for missingness summaries -------------------------------

get_missing_count <- function(data, varname) {
  if (!varname %in% names(data)) {
    warning(paste("Variable not found:", varname))
    return(NA_integer_)
  }
  sum(is.na(data[[varname]]))
}


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
    chronicw5 = na_if(chronicw5, 97),
    chronicw5 = na_if(chronicw5, 98),
    chronicw5 = na_if(chronicw5, 99),
    
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
    educational_level = na_if(educational_level, -1),
    educational_level = na_if(educational_level, -2),
    educational_level = na_if(educational_level, 97),
    educational_level = na_if(educational_level, 98),
    educational_level = na_if(educational_level, 99),
    
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
    employment_status = na_if(employment_status, -1),
    employment_status = na_if(employment_status, -2),
    employment_status = na_if(employment_status, 97),
    employment_status = na_if(employment_status, 98),
    employment_status = na_if(employment_status, 99),
    
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
      smoke_present == 1 ~ "smoking",
      smoke_present == 5 ~ "non_smoking",
      is.na(smoke_present) | smoke_present %in% c(-1, -2, 97, 98, 99) ~ "unknown",
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
# Missingness percentages for key modelling variables
# ============================================================

missingness_table <- tibble(
  variable = c(
    "Smoking status",
    "Marital status",
    "Employment status",
    "Education level",
    "Chronic condition count",
    "Alcohol consumption",
    "Moderate physical activity",
    "Vigorous physical activity",
    "EURO-D caseness W6",
    "Age",
    "Gender"
  ),
  
  n_missing = c(
    sum(missing_analysis_data$smoke_3cat == "unknown", na.rm = TRUE),
    sum(missing_analysis_data$marital_3cat == "missing", na.rm = TRUE),
    sum(is.na(missing_analysis_data$employment_3cat)),
    sum(is.na(missing_analysis_data$education_3cat)),
    sum(is.na(missing_analysis_data$chronicw5)),
    get_missing_count(missing_analysis_data, alcohol_var),
    get_missing_count(missing_analysis_data, moderate_pa_var),
    get_missing_count(missing_analysis_data, vigorous_pa_var),
    sum(is.na(missing_analysis_data$eurodcat_w6)),
    sum(is.na(missing_analysis_data$age2013)),
    sum(is.na(missing_analysis_data$gender))
  ),
  
  n_total = nrow(missing_analysis_data)
) %>%
  mutate(
    percent_missing = round((n_missing / n_total) * 100, 2)
  ) %>%
  arrange(desc(percent_missing))

print(missingness_table)

write_csv(missingness_table, "missingness_table_key_variables.csv")


# ============================================================
# Optional: inspect possible alcohol and physical activity names
# ============================================================

possible_alcohol_pa_vars <- names(panel_full)[
  str_detect(
    names(panel_full),
    regex("alcohol|drink|br010|physical|activity|moderate|vigorous|br015|br016", ignore_case = TRUE)
  )
]

print(possible_alcohol_pa_vars)


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
# Education is not included as predictor because education
# missingness is the outcome
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


# ============================================================
# Extract odds ratios from missingness models
# ============================================================

missingness_model_results <- bind_rows(
  tidy(missing_smoking_model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(missingness_outcome = "Smoking status missing"),
  
  tidy(missing_marital_model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(missingness_outcome = "Marital status missing"),
  
  tidy(missing_employment_model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(missingness_outcome = "Employment status missing"),
  
  tidy(missing_education_model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(missingness_outcome = "Education level missing")
) %>%
  select(
    missingness_outcome,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = round(p.value, 4)
  )

print(missingness_model_results)

write_csv(missingness_model_results, "missingness_logistic_models_ORs.csv")


# ============================================================
# Compact table: significant predictors of missingness
# ============================================================

significant_missingness_predictors <- missingness_model_results %>%
  filter(term != "(Intercept)") %>%
  filter(p.value < 0.05) %>%
  arrange(missingness_outcome, p.value)

print(significant_missingness_predictors)

write_csv(
  significant_missingness_predictors,
  "significant_predictors_of_missingness.csv"
)