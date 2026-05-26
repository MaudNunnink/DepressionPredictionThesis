


# ============================================================
# 06_xgboost_chronic_tuned.R
# XGBoost tuned model: individual chronic conditions
# Fixed class weighting via scale_pos_weight
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

print(table(xgb_data_chronic$eurodcat_w6, useNA = "ifany"))

# Train-test split --------------------------------------------------------

set.seed(123)

index <- createDataPartition(
  xgb_data_chronic$eurodcat_w6,
  p = 0.8,
  list = FALSE
)

train_data <- xgb_data_chronic[index, ]
test_data  <- xgb_data_chronic[-index, ]

print(table(train_data$eurodcat_w6))
print(table(test_data$eurodcat_w6))

# Model matrices ----------------------------------------------------------

x_train <- model.matrix(eurodcat_w6 ~ . - 1, data = train_data)
x_test  <- model.matrix(eurodcat_w6 ~ . - 1, data = test_data)

y_train <- ifelse(train_data$eurodcat_w6 == "yes", 1, 0)
y_test  <- ifelse(test_data$eurodcat_w6 == "yes", 1, 0)

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test, label = y_test)

# Class weighting ---------------------------------------------------------

scale_pos_weight_value <- sum(y_train == 0) / sum(y_train == 1)

print(scale_pos_weight_value)

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

# 10-fold CV tuning -------------------------------------------------------

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
    verbose = 1
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

print(tuning_results)

# Final tuned model -------------------------------------------------------

best_params <- tuning_results %>% slice(1)
print(best_params)

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

xgb_chronic_tuned <- xgb.train(
  params = params_final,
  data = dtrain,
  nrounds = best_params$best_iteration,
  verbose = 1
)

# Determine Youden threshold on TRAINING set ------------------------------

train_pred_probs <- predict(
  xgb_chronic_tuned,
  newdata = dtrain
)

train_observed <- factor(
  ifelse(y_train == 1, "yes", "no"),
  levels = c("no", "yes")
)

roc_train <- roc(
  response = train_observed,
  predictor = train_pred_probs,
  levels = c("no", "yes"),
  direction = "<"
)

youden_coords_train <- coords(
  roc_train,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

print(youden_coords_train)

youden_threshold <- as.numeric(youden_coords_train["threshold"])
print(youden_threshold)

# Predict on TEST set -----------------------------------------------------

pred_probs <- predict(
  xgb_chronic_tuned,
  newdata = dtest
)

observed <- factor(
  ifelse(y_test == 1, "yes", "no"),
  levels = c("no", "yes")
)

# Classification using default threshold 0.50 -----------------------------

pred_class_050 <- factor(
  ifelse(pred_probs >= 0.5, "yes", "no"),
  levels = c("no", "yes")
)

cm_050 <- confusionMatrix(
  data = pred_class_050,
  reference = observed,
  positive = "yes"
)

print(cm_050)

# Classification using Youden threshold ----------------------------------

pred_class_youden <- factor(
  ifelse(pred_probs >= youden_threshold, "yes", "no"),
  levels = c("no", "yes")
)

cm_youden <- confusionMatrix(
  data = pred_class_youden,
  reference = observed,
  positive = "yes"
)

print(cm_youden)

# ROC and AUC on TEST set -------------------------------------------------

roc_test <- roc(
  response = observed,
  predictor = pred_probs,
  levels = c("no", "yes"),
  direction = "<"
)

auc_value <- as.numeric(auc(roc_test))
print(auc_value)

auc_ci_boot <- ci.auc(
  roc_test,
  method = "bootstrap",
  boot.n = 2000
)

print(auc_ci_boot)

# Helper function for performance metrics --------------------------------

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

# Performance tables ------------------------------------------------------

model_performance_xgb_chronic_050 <- extract_performance(
  cm = cm_050,
  model_name = "XGBoost - individual chronic conditions",
  threshold_label = "Default 0.50",
  threshold_value = 0.5,
  auc_value = auc_value
)

model_performance_xgb_chronic_youden <- extract_performance(
  cm = cm_youden,
  model_name = "XGBoost - individual chronic conditions",
  threshold_label = "Youden",
  threshold_value = youden_threshold,
  auc_value = auc_value
)

model_performance_xgb_chronic_comparison <- bind_rows(
  model_performance_xgb_chronic_050,
  model_performance_xgb_chronic_youden
)

print(model_performance_xgb_chronic_comparison)

# SHAP analysis -----------------------------------------------------------


# Calculate SHAP values on TEST set
shap_values <- predict(
  xgb_chronic_tuned,
  newdata = dtest,
  predcontrib = TRUE
)

shap_values <- as.data.frame(shap_values)

# Remove Intercept term for feature importance
shap_feature_values <- shap_values %>%
  dplyr::select(-`(Intercept)`)

# Mean absolute SHAP values = global feature importance
shap_importance <- shap_feature_values %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "mean_abs_shap"
  ) %>%
  arrange(desc(mean_abs_shap))

print(shap_importance)


# Clean feature names -----------------------------------------------------

table(analysis_data_complete$gender)

feature_labels <- c(
  gender1 = "Male sex",
  gender2 = "Female sex",
  other_affective = "Other affective disorder",
  education_3cathigh = "High education",
  education_3catmid = "Medium education",
  rheumatoid_arthritis = "Rheumatoid arthritis",
  age2013 = "Age",
  other_chronic = "Other chronic condition",
  employment_3catnot_working = "Not working",
  chronic_lung_disease = "Chronic lung disease",
  osteoarthritis = "Osteoarthritis",
  cancer = "Cancer",
  high_blood_pressure = "High blood pressure",
  high_blood_cholesterol = "High blood cholesterol",
  heart_attack = "Heart attack",
  mod_activity_3catmoderate = "Moderate physical activity",
  mod_activity_3cathigh = "High physical activity",
  ulcer = "Ulcer",
  stroke = "Stroke",
  alcohol_per_week = "Alcohol consumption"
)

# Top 20 SHAP features ----------------------------------------------------

shap_top20 <- shap_importance %>%
  slice_max(mean_abs_shap, n = 20) %>%
  mutate(
    feature_clean = recode(feature, !!!feature_labels),
    feature_clean = reorder(feature_clean, mean_abs_shap)
  )

# Improved SHAP plot ------------------------------------------------------

p_shap <- ggplot(
  shap_top20,
  aes(x = feature_clean, y = mean_abs_shap, fill = mean_abs_shap)
) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_gradient(
    low = "#90CAF9",
    high = "#1565C0"
  ) +
  labs(
    title = "SHAP Feature Importance for the XGBoost Model",
    x = NULL,
    y = "Mean absolute SHAP value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 20)
  )

print(p_shap)

# Save higher-quality figure ----------------------------------------------

ggsave(
  filename = "shap_importance_xgb_chronic.png",
  plot = p_shap,
  width = 10,
  height = 7,
  dpi = 300
)

# Save SHAP importance table
saveRDS(
  shap_importance,
  "shap_importance_xgb_chronic.rds"
)

# Optional: save plot
ggsave(
  filename = "shap_importance_xgb_chronic.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Variable importance -----------------------------------------------------

varimp_xgb_chronic <- xgb.importance(
  feature_names = colnames(x_train),
  model = xgb_chronic_tuned
) %>%
  as_tibble()

print(varimp_xgb_chronic)

# Save outputs ------------------------------------------------------------

saveRDS(xgb_chronic_tuned, "xgb_chronic_tuned_model.rds")
saveRDS(tuning_results, "tuning_results_xgb_chronic.rds")
saveRDS(best_params, "best_params_xgb_chronic.rds")

saveRDS(youden_coords_train, "youden_coords_train_xgb_chronic.rds")
saveRDS(youden_threshold, "youden_threshold_xgb_chronic.rds")

saveRDS(model_performance_xgb_chronic_youden, "model_performance_xgb_chronic_youden.rds")
saveRDS(model_performance_xgb_chronic_050, "model_performance_xgb_chronic_050.rds")
saveRDS(model_performance_xgb_chronic_comparison, "model_performance_xgb_chronic_comparison.rds")

saveRDS(cm_youden, "confusion_matrix_xgb_chronic_youden.rds")
saveRDS(cm_050, "confusion_matrix_xgb_chronic_050.rds")
saveRDS(roc_test, "roc_xgb_chronic.rds")
saveRDS(auc_ci_boot, "auc_ci_xgb_chronic.rds")
saveRDS(varimp_xgb_chronic, "varimp_xgb_chronic.rds")