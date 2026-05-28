01_dataset_construction.R | Load SHARE wave 5 and 6 modules, restricts the sample to Germany, links respondents across waves, applies inclusion and exclusion criteria 

02a_missingness_analysis.R | Creates a simplified analysis dataset from the constructed panel data and examines whether missingness in key covariates is associated with demographic, health-related, and outcome variables.

02b_data_preprocessing.R | Renames and recodes variables for the main analysis, including recategorization, missing value handling, and the creation of the final analytical dataset.

03_latent_class_analysis.R | Performs Latent Class Analysis (LCA) on chronic disease indicators, compares model fit across class solutions, visualises conditional item-response probabilities, and merges LCA-derived features back into the analytical dataset.

