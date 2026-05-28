

# ============================================================
# 06a_Logistic regression: sex and age groups
# Individual chronic conditions
# ============================================================

View(glm_chronic_subgroup_performance)

library(tidyverse)
library(caret)
library(pROC)
library(broom)

analysis_data_complete <- readRDS("analysis_data_complete.rds")

chronic_disease_vars <- c(
  "heart_attack", "high_blood_pressure", "high_blood_cholesterol",
  "stroke", "diabetes", "chronic_lung_disease", "cancer", "ulcer",
  "parkinson", "cataracts", "hip_fracture", "alzheimers",
  "other_affective", "rheumatoid_arthritis", "osteoarthritis",
  "other_chronic"
)

common_predictors <- c(
  "gender", "age2013", "marital_3cat", "employment_3cat",
  "education_3cat", "smoke_3cat", "alcohol_per_week",
  "vig_activity_3cat", "mod_activity_3cat"
)

glm_data_chronic <- analysis_data_complete %>%
  dplyr::select(
    eurodcat_w6,
    all_of(common_predictors),
    all_of(chronic_disease_vars)
  ) %>%
  mutate(
    eurodcat_w6 = factor(eurodcat_w6, levels = c("no", "yes"))
  ) %>%
  drop_na()

print(table(glm_data_chronic$eurodcat_w6, useNA = "ifany"))
print(table(glm_data_chronic$gender, useNA = "ifany"))

extract_performance <- function(cm, model_name, threshold_label, threshold_value, auc_value) {
  
  precision <- as.numeric(cm$byClass["Pos Pred Value"])
  recall <- as.numeric(cm$byClass["Sensitivity"])
  
  f1_score <- ifelse(
    is.na(precision) | is.na(recall) | (precision + recall == 0),
    NA,
    2 * ((precision * recall) / (precision + recall))
  )
  
  tibble(
    model = model_name,
    threshold_type = threshold_label,
    threshold = threshold_value,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    balanced_accuracy = as.numeric(cm$byClass["Balanced Accuracy"]),
    sensitivity = as.numeric(cm$byClass["Sensitivity"]),
    specificity = as.numeric(cm$byClass["Specificity"]),
    precision = precision,
    f1_score = f1_score,
    cohens_kappa = as.numeric(cm$overall["Kappa"]),
    auc = auc_value
  )
}

run_glm_subgroup <- function(data, subgroup_name, remove_gender = FALSE) {
  
  cat("\n============================================================\n")
  cat("Running subgroup:", subgroup_name, "\n")
  cat("============================================================\n")
  
  print(table(data$eurodcat_w6))
  
  predictors <- common_predictors
  
  if (remove_gender) {
    predictors <- setdiff(predictors, "gender")
  }
  
  model_data <- data %>%
    dplyr::select(
      eurodcat_w6,
      all_of(predictors),
      all_of(chronic_disease_vars)
    ) %>%
    drop_na()
  
  set.seed(123)
  
  index <- createDataPartition(
    model_data$eurodcat_w6,
    p = 0.8,
    list = FALSE
  )
  
  train_data <- model_data[index, ]
  test_data  <- model_data[-index, ]
  
  class_weights <- ifelse(
    train_data$eurodcat_w6 == "yes",
    sum(train_data$eurodcat_w6 == "no") / sum(train_data$eurodcat_w6 == "yes"),
    1
  )
  
  train_control <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  set.seed(123)
  
  glm_model <- train(
    eurodcat_w6 ~ .,
    data = train_data,
    method = "glm",
    family = binomial,
    weights = class_weights,
    trControl = train_control,
    metric = "ROC"
  )
  
  train_pred_probs <- predict(
    glm_model,
    newdata = train_data,
    type = "prob"
  )[, "yes"]
  
  roc_train <- roc(
    response = train_data$eurodcat_w6,
    predictor = train_pred_probs,
    levels = c("no", "yes"),
    quiet = TRUE
  )
  
  youden_coords_train <- coords(
    roc_train,
    x = "best",
    best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity")
  )
  
  youden_threshold <- as.numeric(youden_coords_train["threshold"])
  
  pred_probs <- predict(
    glm_model,
    newdata = test_data,
    type = "prob"
  )[, "yes"]
  
  observed <- factor(
    test_data$eurodcat_w6,
    levels = c("no", "yes")
  )
  
  pred_class_050 <- factor(
    ifelse(pred_probs >= 0.5, "yes", "no"),
    levels = c("no", "yes")
  )
  
  pred_class_youden <- factor(
    ifelse(pred_probs >= youden_threshold, "yes", "no"),
    levels = c("no", "yes")
  )
  
  cm_050 <- confusionMatrix(
    data = pred_class_050,
    reference = observed,
    positive = "yes"
  )
  
  cm_youden <- confusionMatrix(
    data = pred_class_youden,
    reference = observed,
    positive = "yes"
  )
  
  roc_test <- roc(
    response = observed,
    predictor = pred_probs,
    levels = c("no", "yes"),
    quiet = TRUE
  )
  
  auc_value <- as.numeric(auc(roc_test))
  
  perf_050 <- extract_performance(
    cm = cm_050,
    model_name = subgroup_name,
    threshold_label = "Default 0.50",
    threshold_value = 0.5,
    auc_value = auc_value
  )
  
  perf_youden <- extract_performance(
    cm = cm_youden,
    model_name = subgroup_name,
    threshold_label = "Youden",
    threshold_value = youden_threshold,
    auc_value = auc_value
  )
  
  coefficients <- broom::tidy(
    glm_model$finalModel,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(
      subgroup = subgroup_name,
      across(where(is.numeric), ~ round(.x, 3))
    )
  
  return(list(
    model = glm_model,
    performance = bind_rows(perf_050, perf_youden),
    coefficients = coefficients,
    youden_threshold = youden_threshold,
    test_predictions = tibble(
      observed = observed,
      predicted_probability = pred_probs,
      pred_class_050 = pred_class_050,
      pred_class_youden = pred_class_youden
    )
  ))
}

# Create subgroup datasets ------------------------------------------------

female_data <- glm_data_chronic %>%
  filter(gender == 2 | gender == "female" | gender == "Female")

male_data <- glm_data_chronic %>%
  filter(gender == 1 | gender == "male" | gender == "Male")

age_50_64_data <- glm_data_chronic %>%
  filter(age2013 >= 50, age2013 <= 64)

age_65_74_data <- glm_data_chronic %>%
  filter(age2013 >= 65, age2013 <= 74)

age_75plus_data <- glm_data_chronic %>%
  filter(age2013 >= 75)

# Run subgroup models -----------------------------------------------------

glm_chronic_female <- run_glm_subgroup(
  data = female_data,
  subgroup_name = "LR ICC - Female",
  remove_gender = TRUE
)

glm_chronic_male <- run_glm_subgroup(
  data = male_data,
  subgroup_name = "LR ICC - Male",
  remove_gender = TRUE
)

glm_chronic_age_50_64 <- run_glm_subgroup(
  data = age_50_64_data,
  subgroup_name = "LR ICC - Age 50-64",
  remove_gender = FALSE
)

glm_chronic_age_65_74 <- run_glm_subgroup(
  data = age_65_74_data,
  subgroup_name = "LR ICC - Age 65-74",
  remove_gender = FALSE
)

glm_chronic_age_75plus <- run_glm_subgroup(
  data = age_75plus_data,
  subgroup_name = "LR ICC - Age 75+",
  remove_gender = FALSE
)

# Combine performance results --------------------------------------------

glm_chronic_subgroup_performance <- bind_rows(
  glm_chronic_female$performance,
  glm_chronic_male$performance,
  glm_chronic_age_50_64$performance,
  glm_chronic_age_65_74$performance,
  glm_chronic_age_75plus$performance
)

print(glm_chronic_subgroup_performance)

glm_chronic_subgroup_performance_youden <- glm_chronic_subgroup_performance %>%
  filter(threshold_type == "Youden")

print(glm_chronic_subgroup_performance_youden)

glm_chronic_subgroup_performance_050 <- glm_chronic_subgroup_performance %>%
  filter(threshold_type == "Default 0.50")

print(glm_chronic_subgroup_performance_050)

# Combine coefficients ----------------------------------------------------

glm_chronic_subgroup_coefficients <- bind_rows(
  glm_chronic_female$coefficients,
  glm_chronic_male$coefficients,
  glm_chronic_age_50_64$coefficients,
  glm_chronic_age_65_74$coefficients,
  glm_chronic_age_75plus$coefficients
)

print(glm_chronic_subgroup_coefficients, n = Inf)

# Save results ------------------------------------------------------------

saveRDS(
  glm_chronic_subgroup_performance,
  "glm_chronic_subgroup_performance_all_thresholds.rds"
)

saveRDS(
  glm_chronic_subgroup_performance_youden,
  "glm_chronic_subgroup_performance_youden.rds"
)

saveRDS(
  glm_chronic_subgroup_performance_050,
  "glm_chronic_subgroup_performance_default_050.rds"
)

saveRDS(
  glm_chronic_subgroup_coefficients,
  "glm_chronic_subgroup_coefficients.rds"
)

write.csv(
  glm_chronic_subgroup_performance,
  "glm_chronic_subgroup_performance_all_thresholds.csv",
  row.names = FALSE
)

write.csv(
  glm_chronic_subgroup_performance_youden,
  "glm_chronic_subgroup_performance_youden.csv",
  row.names = FALSE
)

write.csv(
  glm_chronic_subgroup_coefficients,
  "glm_chronic_subgroup_coefficients.csv",
  row.names = FALSE
)