01_dataset_construction.R | Load SHARE wave 5 and 6 modules, restricts the sample to Germany, links respondents across waves, applies inclusion and exclusion criteria 

02a_missingness_analysis.R | Creates a simplified analysis dataset from the constructed panel data and examines whether missingness in key covariates is associated with demographic, health-related, and outcome variables.

02b_data_preprocessing.R | Renames and recodes variables for the main analysis, including recategorization, missing value handling, and the creation of the final analytical dataset.

03_latent_class_analysis.R | Performs Latent Class Analysis (LCA) on chronic disease indicators, compares model fit across class solutions, visualises conditional item-response probabilities, and merges LCA-derived features back into the analytical dataset.

04_logistic regression | All logistic regression scripts include train-test splitting, cross-validation, class weighting, ROC/AUC evaluation, threshold comparison, and performance metric extraction. Adjusted odds ratios are additionally exported for model interpretation.

04a_logistic_regression_chronic.R | Trains and evaluates a logistic regression model using individual chronic disease indicators 

04b_logistic_regression_condition_count.R | Trains and evaluates a logistic regression model using chronic condition counts 

04c_logistic_regression_LCA_features.R | Trains and evaluates a logistic regression model using LCA features 

05_XGBoost | All XGBoost scripts include train-test splitting, hyperparameter tuning using cross-validation, class imbalance handling, ROC/AUC evaluation, threshold comparison, performance metric extraction, and SHAP-based feature importance analysis.

05a_XGBoost_individual_conditions_with_class_weights | Trains and evaluates a XGboost model using chronic disease indicators 

05b_XGBoost


