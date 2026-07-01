
# ============================================================
# Figure X. Top five predictors based on mean absolute SHAP values
# across XGBoost models
# ============================================================

library(tidyverse)
library(xgboost)
library(caret)
library(grid)
library(stringr)

# ------------------------------------------------------------
# Feature labels used across the three XGBoost models
# ------------------------------------------------------------

feature_labels <- c(
  # Sociodemographic variables
  gender1 = "Male sex",
  gender2 = "Female sex",
  age2013 = "Age",
  marital_3catnot_married = "Not married",
  marital_3catwidowed = "Widowed",
  marital_3catmissing = "Marital status missing",
  employment_3catretired = "Retired",
  employment_3catnot_working = "Not working",
  education_3catmid = "Medium education",
  education_3cathigh = "High education",
  
  # Lifestyle variables
  smoke_3catsmoking = "Current smoking",
  smoke_3catunknown = "Smoking status unknown",
  alcohol_per_week = "Alcohol consumption",
  vig_activity_3catmoderate = "Moderate vigorous activity",
  vig_activity_3cathigh = "High vigorous activity",
  mod_activity_3catmoderate = "Moderate physical activity",
  mod_activity_3cathigh = "High physical activity",
  
  # Individual chronic conditions
  heart_attack = "Heart attack",
  high_blood_pressure = "High blood pressure",
  high_blood_cholesterol = "High blood cholesterol",
  stroke = "Stroke",
  diabetes = "Diabetes",
  chronic_lung_disease = "Chronic lung disease",
  cancer = "Cancer",
  ulcer = "Ulcer",
  parkinson = "Parkinson's disease",
  cataracts = "Cataracts",
  hip_fracture = "Hip fracture",
  alzheimers = "Alzheimer's disease",
  other_affective = "Other affective disorder",
  rheumatoid_arthritis = "Rheumatoid arthritis",
  osteoarthritis = "Osteoarthritis",
  other_chronic = "Other chronic condition",
  
  # Chronic condition count
  chronic_cat1 = "1 chronic condition",
  chronic_cat2 = "2 chronic conditions",
  chronic_cat3 = "3 chronic conditions",
  `chronic_cat4+` = "4+ chronic conditions",
  
  # LCA-derived features
  lca_labelLow.morbidity = "Low-morbidity class",
  `lca_labelLow morbidity` = "Low-morbidity class",
  lca_labelCardiometabolic = "Cardiometabolic class",
  lca_labelMusculoskeletal = "Musculoskeletal class"
)

# ------------------------------------------------------------
# Helper function for cleaning SHAP importance tables
# ------------------------------------------------------------

clean_shap_importance <- function(data, model_name) {
  data %>%
    mutate(
      model_type = model_name,
      feature_clean = recode(feature, !!!feature_labels, .default = feature)
    )
}

# ------------------------------------------------------------
# Read existing SHAP importance outputs
# ------------------------------------------------------------

shap_chronic <- readRDS("shap_importance_xgb_chronic.rds") %>%
  clean_shap_importance("Individual conditions model")

shap_count <- readRDS("shap_importance_xgb_count.rds") %>%
  clean_shap_importance("Condition count model")

# ------------------------------------------------------------
# Read LCA SHAP importance if available.
# If not available, recompute SHAP values using the saved LCA XGBoost model.
# This does NOT retrain the model.
# ------------------------------------------------------------

if (file.exists("shap_importance_xgb_lca.rds")) {
  
  shap_lca <- readRDS("shap_importance_xgb_lca.rds") %>%
    clean_shap_importance("LCA-derived features model")
  
} else {
  
  message("shap_importance_xgb_lca.rds not found. Recomputing SHAP importance from saved LCA model.")
  
  # Load data and model
  analysis_data_lca <- readRDS("analysis_data_lca.rds")
  xgb_lca_tuned <- readRDS("xgb_lca_tuned_model.rds")
  
  common_predictors <- c(
    "gender", "age2013", "marital_3cat", "employment_3cat",
    "education_3cat", "smoke_3cat", "alcohol_per_week",
    "vig_activity_3cat", "mod_activity_3cat"
  )
  
  xgb_data_lca <- analysis_data_lca %>%
    dplyr::select(
      eurodcat_w6,
      all_of(common_predictors),
      lca_label
    ) %>%
    mutate(
      eurodcat_w6 = factor(eurodcat_w6, levels = c("no", "yes")),
      lca_label = factor(
        lca_label,
        levels = c("Low morbidity", "Cardiometabolic", "Musculoskeletal")
      )
    ) %>%
    drop_na()
  
  # Recreate the same train-test split as in 05c_xgboost_lca_tuned.R
  set.seed(123)
  
  index <- createDataPartition(
    xgb_data_lca$eurodcat_w6,
    p = 0.8,
    list = FALSE
  )
  
  test_data <- xgb_data_lca[-index, ]
  
  x_test <- model.matrix(eurodcat_w6 ~ . - 1, data = test_data)
  dtest <- xgb.DMatrix(data = x_test)
  
  shap_values_lca <- predict(
    xgb_lca_tuned,
    newdata = dtest,
    predcontrib = TRUE
  ) %>%
    as.data.frame()
  
  shap_feature_values_lca <- shap_values_lca %>%
    dplyr::select(-any_of(c("(Intercept)", "BIAS")))
  
  shap_importance_lca <- shap_feature_values_lca %>%
    summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "feature",
      values_to = "mean_abs_shap"
    ) %>%
    arrange(desc(mean_abs_shap))
  
  saveRDS(shap_importance_lca, "shap_importance_xgb_lca.rds")
  
  shap_lca <- shap_importance_lca %>%
    clean_shap_importance("LCA-derived features model")
}

# ------------------------------------------------------------
# Combine SHAP importance tables
# ------------------------------------------------------------

shap_all <- bind_rows(
  shap_chronic,
  shap_count,
  shap_lca
) %>%
  mutate(
    model_type = factor(
      model_type,
      levels = c(
        "Individual conditions model",
        "Condition count model",
        "LCA-derived features model"
      )
    )
  )

# ------------------------------------------------------------
# Select top five predictors per XGBoost model
# ------------------------------------------------------------

top5_shap <- shap_all %>%
  group_by(model_type) %>%
  slice_max(
    order_by = mean_abs_shap,
    n = 5,
    with_ties = FALSE
  ) %>%
  arrange(model_type, mean_abs_shap) %>%
  mutate(
    feature_label = str_wrap(feature_clean, width = 35),
    feature_facet = paste(model_type, feature_label, sep = "___")
  ) %>%
  ungroup() %>%
  mutate(
    feature_facet = factor(
      feature_facet,
      levels = unique(feature_facet)
    )
  )

# Optional: inspect selected features
print(top5_shap)

# ------------------------------------------------------------
# Create top-five SHAP plot
# ------------------------------------------------------------

figure_shap_top5 <- ggplot(
  top5_shap,
  aes(x = mean_abs_shap, y = feature_facet)
) +
  geom_col(width = 0.7) +
  facet_grid(
    model_type ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_y_discrete(
    labels = function(x) str_remove(x, "^.*___")
  ) +
  labs(
    x = "Mean absolute SHAP value",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text.y = element_text(
      face = "bold",
      size = 11,
      angle = 0
    ),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(
      size = 10,
      lineheight = 0.9
    ),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    panel.spacing.y = unit(0.9, "lines"),
    plot.margin = margin(10, 20, 10, 10)
  )

# Show plot
figure_shap_top5

# ------------------------------------------------------------
# Save plot
# ------------------------------------------------------------

ggsave(
  filename = "Figure_X_top5_SHAP_XGBoost_models.png",
  plot = figure_shap_top5,
  width = 8.5,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = "Figure_X_top5_SHAP_XGBoost_models.pdf",
  plot = figure_shap_top5,
  width = 8.5,
  height = 6.5
)