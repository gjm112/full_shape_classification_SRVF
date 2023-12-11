# full_shape_classification_SRVF

Step 1: ./code/R/dataprep.R

Step 2: ./code/matlab/Data_prep_for_full_training_dataset.m This outputs all the data projected onto the tangent space.

Step 2a: ./code/R/elliptical_fourier_feature_generation.R This generates features based on EFA.

Step 3: Cross validation.
./code/R/random_forest_species_given_tribe.R ./code/R/svm_linear_kernel_species_given_tribe.R ./code/R/svm_radial_kernel_species_given_tribe.R

Step 4: ./code/R/results_summary.R

Step 5: Data example with Gladysvale teeth
./code/R/dataprep_for_gladysvale.R
./code/matlab/dataprep_for_gladyvale_teeth_for_prediction.m
./code/R/Gladysvale_Actual_Fossil_Prediction.R ./code/R/Gladysvale_Actual_Fossil_Prediction_Overall_Mean.R

