# Chronic Disease Burden and Depressive Symptoms in Older Adults

Analysis pipeline using SHARE Wave 5 and 6 data (Germany).

## Scripts

| `01_dataset_construction.R` | Loads SHARE Wave 5 and 6 modules, restricts sample to Germany, links respondents across waves, applies inclusion/exclusion criteria |
| `02a_missingness_analysis.R` | Creates a simplified analysis dataset and examines whether missingness in key covariates is associated with demographic, health-related, and outcome variables |
| `02b_data_preprocessing.R` | Renames and recodes variables, handles missing values, and constructs the final analytical dataset |
| `02c_exploratory_data_analysis.R` | Descriptive statistics and exploratory figures on the final analytical sample |
| `03_latent_class_analysis.R` | Performs Latent Class Analysis (LCA) on chronic disease indicators |
| `04a_LR_individual_conditions.R` | Logistic regression using individual chronic disease indicators |
| `04b_LR_condition_count.R` | Logistic regression using chronic condition counts |
| `04c_LR_LCA_features.R` | Logistic regression using LCA class membership as predictor |
| `05a_XGB_individual_conditions.R` | XGBoost model using individual chronic disease indicators (with class weights) |
| `05b_XGB_condition_counts.R` | XGBoost model using chronic condition counts |
| `05c_XGB_LCA_features.R` | XGBoost model using LCA class membership as predictor |
| `06a_LR_individual_conditions_sex_age.R` | Sex- and age-stratified logistic regression using individual chronic disease indicators |
| `06b_LR_condition_count_sex_age.R` | Sex- and age-stratified logistic regression using chronic condition counts |
| `06c_XGB_individual_conditions_sex_age.R` | Sex- and age-stratified XGBoost models using individual chronic disease indicators |
| `06d_XGB_condition_count_sex_age.R` | Sex- and age-stratified XGBoost models using chronic condition counts |
| `07a_deLong_tests.R` | DeLong tests comparing model AUCs |
| `07b_forest_plots_odds_ratios_top5.R` | Forest plots of top 5 predictors by odds ratio |
| `07c_SHAP_feature_importance_top5.R` | SHAP feature importance plots for top 5 predictors |

## Pipeline order
Scripts are numbered in execution order (`01` → `07c`). Run sequentially.

