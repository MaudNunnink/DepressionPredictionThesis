


# ============================================================
# Missingness analysis
# ============================================================

# Create dataset for missingness analysis
missing_analysis_data <- analysis_data %>%
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
# Education not included as predictor because it is outcome
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