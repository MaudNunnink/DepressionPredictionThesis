01_dataset_construction.R | Load SHARE wave 5 and 6 modules, restricts the sample to Germany, links respondents across waves, applies inclusion and exclusion criteria 

02a_missingness_analysis.R | Creates a simplified analysis dataset from the constructed panel data and examines whether missingness in key covariates is associated with demographic, health-related, and outcome variables.

02b_data_preprocessing.R | Renames and recodes variables for the main analysis, including recategorization, missing value handling, and the creation of the final analytical dataset.

02c_exploratory_data_analysis.R | Exploratory data analysis (EDA) on the final analytical sample, including descriptive statistics, demographic distributions, chronic disease burden analyses, prevalence estimates, and visualizations stratified by age group and sex. Outputs include tables and figures used in the thesis and appendix.

03_latent_class_analysis.R | Performs Latent Class Analysis (LCA) on chronic disease indicators, compares model fit across class solutions, visualises conditional item-response probabilities, and merges LCA-derived features back into the analytical dataset.

04: logistic regression | All logistic regression scripts include train-test splitting, cross-validation, class weighting, ROC/AUC evaluation, threshold comparison, and performance metric extraction. Adjusted odds ratios are additionally exported for model interpretation.

04a_logistic_regression_chronic.R | Trains and evaluates a logistic regression model using individual chronic disease indicators 

04b_logistic_regression_condition_count.R | Trains and evaluates a logistic regression model using chronic condition counts 

04c_logistic_regression_LCA_features.R | Trains and evaluates a logistic regression model using LCA features 

05: XGBoost | All XGBoost scripts include train-test splitting, hyperparameter tuning using cross-validation, class imbalance handling, ROC/AUC evaluation, threshold comparison, performance metric extraction, and SHAP-based feature importance analysis.

05a_XGBoost_individual_conditions_with_class_weights | Trains and evaluates a XGboost model using chronic disease indicators 

05b_XGBoost_chronic_condition_count |Trains and evaluates a XGBoost model using chronic condition counts

05c_XGBoost_LCA_features|Trains and evaluates a XGBoost model using the LCA classes as predictors

06: Sex-and-Age specific models | All subgroup scripts train and evaluate models separately by sex and age group, using the same modelling workflow as the main analyses, including train-test splitting, threshold comparison, ROC/AUC evaluation, and performance metric extraction.

06a_logistic_regression_individual_indicators_sex_age_group | Trains and evaluates sex- and age-specific logistic regression models using individual chronic disease indicators.

06b_logistic_regression_condition_count_sex_age_group | Trains and evaluates sex- and age-specific logistic regression models using chronic condition counts.

06c_XGBoost_individual_indicators_sex_age_group | Trains and evaluates sex- and age-specific XGBoost models using individual chronic disease indicators.

06d_XGBoost_condition_count_sex_age_group | Trains and evaluates sex- and age-specific XGBoost models using chronic condition counts.




