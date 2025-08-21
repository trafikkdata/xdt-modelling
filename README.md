This repository contains the data, functions and scripts to model traffic volume at different temporal resolutions (so far only AADT). 

```
xdt-modelling/
├── README.md
├── config/
│   ├── data_config.yaml           # Data paths, column specifications
│   ├── preprocessing_config.yaml  # List processing rules, scaling params
│   └── model_config.yaml          # Model parameters, hyperparameters
├── R/
│   ├── data_preprocessing.R       # Data preprocessing
│   ├── exploratory_analysis.R     # Functions for exploring and plotting the data
│   ├── feature_engineering.R      # Creating new variables, scaling, etc.
│   ├── model_training.R           # Model fitting functions
│   ├── model_validation.R         # Cross-validation, model selection, performance metrics
|   ├── visualization.R            # Plotting functions
│   └── utilities.R                # Helper functions, data type conversions
├── scripts/
│   ├── 01_data_preprocessing.R    # Execute preprocessing pipeline
│   ├── 02_exploratory_analysis.R  # EDA, data quality checks
│   ├── 03_model_selection.R       # Execute validation/selection
│   ├── 04_run_model.R             # Train final model, save artifacts
│   └── sandbox/                   # Small scripts for exploration and testing
├── tests/
│   ├── test_preprocessing.R       # Unit tests for preprocessing
│   ├── test_models.R              # Model validation tests
│   └── test_utilities.R           # Test helper functions
├── results/
│   ├── predictions/               # Model outputs
│   ├── validation/                # CV results, model comparisons
│   └── figures/                   # Generated plots, diagnostics
├── data/
│   ├── processed/                 # Cleaned, preprocessed data
│   └── raw/                       # Original data files (read-only)
└── docs/
    ├── data_dictionary.rmd        # Variable descriptions
    ├── methodology.rmd            # Statistical approach, assumptions
    └── model_results.rmd          # Final model performance
```
