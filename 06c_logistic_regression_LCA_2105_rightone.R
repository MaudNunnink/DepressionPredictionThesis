


# ============================================================
# 05c_logistic_regression_lca.R
# Logistic regression model: LCA modal class assignment
# ============================================================

library(tidyverse)
library(caret)
library(pROC)
library(broom)
library(flextable)
library(officer)

# Load data ---------------------------------------------------------------

analysis_data_lca <- readRDS("analysis_data_lca.rds")

stopifnot("eurodcat_w6" %in% names(analysis_data_lca))
stopifnot("lca_label" %in% names(analysis_data_lca))

# Define predictors -------------------------------------------------------

common_predictors <- c(
  "gender",
  "age2013",
  "marital_3cat",
  "employment_3cat",
  "education_3cat",
  "smoke_3cat",
  "alcohol_per_week",
  "vig_activity_3cat",
  "mod_activity_3cat"
)

# Create modelling dataset ------------------------------------------------

glm_data_lca <- analysis_data_lca %>%
  dplyr::select(
    eurodcat_w6,
    all_of(common_predictors),
    lca_label
  ) %>%
  mutate(
    eurodcat_w6 = factor(eurodcat_w6, levels = c("no", "yes")),
    lca_label = factor(
      lca_label,
      levels = c(
        "Low morbidity",
        "Cardiometabolic",
        "Musculoskeletal"
      )
    )
  ) %>%
  drop_na()

# Check distributions -----------------------------------------------------

print(table(glm_data_lca$eurodcat_w6, useNA = "ifany"))
print(table(glm_data_lca$lca_label, useNA = "ifany"))

# Train-test split --------------------------------------------------------

set.seed(123)

index <- createDataPartition(
  glm_data_lca$eurodcat_w6,
  p = 0.8,
  list = FALSE
)

train_data <- glm_data_lca[index, ]
test_data  <- glm_data_lca[-index, ]

print(table(train_data$eurodcat_w6))
print(table(test_data$eurodcat_w6))

# Class weights -----------------------------------------------------------

class_weights <- ifelse(
  train_data$eurodcat_w6 == "yes",
  sum(train_data$eurodcat_w6 == "no") / sum(train_data$eurodcat_w6 == "yes"),
  1
)

print(table(class_weights))

# Model training ----------------------------------------------------------

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(123)

glm_cv_lca <- train(
  eurodcat_w6 ~ .,
  data = train_data,
  method = "glm",
  family = binomial,
  weights = class_weights,
  trControl = train_control,
  metric = "ROC"
)

print(glm_cv_lca)

# Determine Youden threshold on TRAINING set ------------------------------

train_pred_probs <- predict(
  glm_cv_lca,
  newdata = train_data,
  type = "prob"
)[, "yes"]

roc_train <- roc(
  response = train_data$eurodcat_w6,
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
  glm_cv_lca,
  newdata = test_data,
  type = "prob"
)[, "yes"]

observed <- factor(
  test_data$eurodcat_w6,
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

model_performance_glm_lca_050 <- extract_performance(
  cm = cm_050,
  model_name = "Logistic regression - LCA modal class",
  threshold_label = "Default 0.50",
  threshold_value = 0.5,
  auc_value = auc_value
)

model_performance_glm_lca_youden <- extract_performance(
  cm = cm_youden,
  model_name = "Logistic regression - LCA modal class",
  threshold_label = "Youden",
  threshold_value = youden_threshold,
  auc_value = auc_value
)

model_performance_glm_lca_comparison <- bind_rows(
  model_performance_glm_lca_050,
  model_performance_glm_lca_youden
)

print(model_performance_glm_lca_comparison)

# Coefficients / adjusted odds ratios -------------------------------------

glm_lca_results <- broom::tidy(
  glm_cv_lca$finalModel,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(glm_lca_results, n = Inf)

# Export coefficient table to Word ----------------------------------------

ft_lca <- flextable(glm_lca_results) %>%
  autofit()

save_as_docx(
  "Logistic regression - LCA modal class" = ft_lca,
  path = "glm_lca_results.docx"
)

# Save outputs ------------------------------------------------------------

saveRDS(glm_cv_lca, "glm_cv_lca.rds")
saveRDS(model_performance_glm_lca_comparison, "model_performance_glm_lca_comparison.rds")
saveRDS(glm_lca_results, "glm_lca_results.rds")