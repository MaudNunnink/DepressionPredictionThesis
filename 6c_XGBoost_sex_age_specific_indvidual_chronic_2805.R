# ============================================================
# XGBoost subgroup models: sex and age groups
# 6c: Individual chronic conditions
# ============================================================


library(tidyverse)
library(xgboost)
library(caret)
library(pROC)

# Load data ---------------------------------------------------------------

analysis_data_complete <- readRDS("analysis_data_complete.rds")

# Define variables --------------------------------------------------------

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

# Create modelling dataset ------------------------------------------------

xgb_data_chronic <- analysis_data_complete %>%
  dplyr::select(
    eurodcat_w6,
    all_of(common_predictors),
    all_of(chronic_disease_vars)
  ) %>%
  mutate(
    eurodcat_w6 = factor(eurodcat_w6, levels = c("no", "yes"))
  ) %>%
  drop_na()

# Check gender coding -----------------------------------------------------

print(table(xgb_data_chronic$gender, useNA = "ifany"))

# Tuning grid -------------------------------------------------------------

xgb_grid_small <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.03, 0.05, 0.10),
  gamma = c(0, 0.5),
  colsample_bytree = c(0.8, 1.0),
  min_child_weight = c(1, 3),
  subsample = c(0.8, 1.0),
  nrounds = c(100, 200)
)

# Helper function ---------------------------------------------------------

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

# Function to train one subgroup model -----------------------------------

run_xgb_subgroup <- function(data, subgroup_name, remove_gender = FALSE) {
  
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
  
  x_train <- model.matrix(eurodcat_w6 ~ . - 1, data = train_data)
  x_test  <- model.matrix(eurodcat_w6 ~ . - 1, data = test_data)
  
  y_train <- ifelse(train_data$eurodcat_w6 == "yes", 1, 0)
  y_test  <- ifelse(test_data$eurodcat_w6 == "yes", 1, 0)
  
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgb.DMatrix(data = x_test, label = y_test)
  
  scale_pos_weight_value <- sum(y_train == 0) / sum(y_train == 1)
  
  tuning_results <- list()
  
  set.seed(123)
  
  for (i in seq_len(nrow(xgb_grid_small))) {
    
    cat("Running tuning model", i, "of", nrow(xgb_grid_small), "\n")
    
    params_cv <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = xgb_grid_small$max_depth[i],
      eta = xgb_grid_small$eta[i],
      gamma = xgb_grid_small$gamma[i],
      colsample_bytree = xgb_grid_small$colsample_bytree[i],
      min_child_weight = xgb_grid_small$min_child_weight[i],
      subsample = xgb_grid_small$subsample[i],
      scale_pos_weight = scale_pos_weight_value
    )
    
    cv_model <- xgb.cv(
      params = params_cv,
      data = dtrain,
      nrounds = xgb_grid_small$nrounds[i],
      nfold = 10,
      stratified = TRUE,
      early_stopping_rounds = 10,
      maximize = TRUE,
      verbose = 0
    )
    
    tuning_results[[i]] <- xgb_grid_small[i, ] %>%
      mutate(
        scale_pos_weight = scale_pos_weight_value,
        best_iteration = which.max(cv_model$evaluation_log$test_auc_mean),
        best_auc = max(cv_model$evaluation_log$test_auc_mean, na.rm = TRUE)
      )
  }
  
  tuning_results <- bind_rows(tuning_results) %>%
    arrange(desc(best_auc))
  
  best_params <- tuning_results %>% slice(1)
  
  params_final <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample,
    scale_pos_weight = scale_pos_weight_value
  )
  
  set.seed(123)
  
  xgb_model <- xgb.train(
    params = params_final,
    data = dtrain,
    nrounds = best_params$best_iteration,
    verbose = 0
  )
  
  # Youden threshold on training set --------------------------------------
  
  train_pred_probs <- predict(xgb_model, newdata = dtrain)
  
  train_observed <- factor(
    ifelse(y_train == 1, "yes", "no"),
    levels = c("no", "yes")
  )
  
  roc_train <- roc(
    response = train_observed,
    predictor = train_pred_probs,
    levels = c("no", "yes"),
    direction = "<",
    quiet = TRUE
  )
  
  youden_coords_train <- coords(
    roc_train,
    x = "best",
    best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity")
  )
  
  youden_threshold <- as.numeric(youden_coords_train["threshold"])
  
  # Prediction on test set ------------------------------------------------
  
  pred_probs <- predict(xgb_model, newdata = dtest)
  
  observed <- factor(
    ifelse(y_test == 1, "yes", "no"),
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
    direction = "<",
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
  
  performance <- bind_rows(perf_050, perf_youden)
  
  return(list(
    model = xgb_model,
    tuning_results = tuning_results,
    best_params = best_params,
    performance = performance,
    test_predictions = tibble(
      observed = observed,
      predicted_probability = pred_probs,
      pred_class_050 = pred_class_050,
      pred_class_youden = pred_class_youden
    )
  ))
}



# Create subgroup datasets ------------------------------------------------
# IMPORTANT:
# Check the table printed above.
# If your gender variable is coded differently, adjust these filters.

female_data <- xgb_data_chronic %>%
  filter(gender == 2 | gender == "female" | gender == "Female")

male_data <- xgb_data_chronic %>%
  filter(gender == 1 | gender == "male" | gender == "Male")

age_50_64_data <- xgb_data_chronic %>%
  filter(age2013 >= 50, age2013 <= 64)

age_65_74_data <- xgb_data_chronic %>%
  filter(age2013 >= 65, age2013 <= 74)

age_75plus_data <- xgb_data_chronic %>%
  filter(age2013 >= 75)

# Run subgroup models -----------------------------------------------------

xgb_female <- run_xgb_subgroup(
  data = female_data,
  subgroup_name = "XGBoost ICC - Female",
  remove_gender = TRUE
)

xgb_male <- run_xgb_subgroup(
  data = male_data,
  subgroup_name = "XGBoost ICC - Male",
  remove_gender = TRUE
)

xgb_age_50_64 <- run_xgb_subgroup(
  data = age_50_64_data,
  subgroup_name = "XGBoost ICC - Age 50-64",
  remove_gender = FALSE
)

xgb_age_65_74 <- run_xgb_subgroup(
  data = age_65_74_data,
  subgroup_name = "XGBoost ICC - Age 65-74",
  remove_gender = FALSE
)

xgb_age_75plus <- run_xgb_subgroup(
  data = age_75plus_data,
  subgroup_name = "XGBoost ICC - Age 75+",
  remove_gender = FALSE
)

# Combine performance results --------------------------------------------

xgb_subgroup_performance <- bind_rows(
  xgb_female$performance,
  xgb_male$performance,
  xgb_age_50_64$performance,
  xgb_age_65_74$performance,
  xgb_age_75plus$performance
)

print(xgb_subgroup_performance)

# Optional: keep only default threshold rows for main subgroup table -------

xgb_subgroup_performance_050 <- xgb_subgroup_performance %>%
  filter(threshold_type == "Default 0.50")

print(xgb_subgroup_performance_050)

# Save results ------------------------------------------------------------

saveRDS(
  xgb_subgroup_performance,
  "xgb_subgroup_performance_all_thresholds.rds"
)

saveRDS(
  xgb_subgroup_performance_050,
  "xgb_subgroup_performance_default_050.rds"
)

write.csv(
  xgb_subgroup_performance,
  "xgb_subgroup_performance_all_thresholds.csv",
  row.names = FALSE
)

write.csv(
  xgb_subgroup_performance_050,
  "xgb_subgroup_performance_default_050.csv",
  row.names = FALSE
)