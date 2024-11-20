# Harmonising MRI-based Metrics (QSM) for Motor Neurone Disease

## Introduction
This project aims to harmonise multi-site MRI-derived quantitative susceptibility mapping (QSM) data for studying Motor Neurone Disease (MND). The focus is on eliminating batch effects using neuroCombat harmonisation method while preserving biological variation. Additionally, the study seeks to correlate QSM metrics with MND subtypes, phenotypes, and progression.

## Requirements
### Tools
- **R**: 4.2 or above
- **RStudio**: Version 2024.04.1+748 (2024.04.1+748) or above
- **[QSMxT](https://qsmxt.github.io/QSMxT/)**: 6.4.4 or above 
- **R Packages**:
  ```[neuroCombat](https://github.com/Jfortin1/neuroCombat_Rpackage)```
  ```[ggseg](https://github.com/ggseg/ggseg)```
  ```dplyr```
  ```ggplot2```
- **Input Data**:
Multi-site QSM datasets with batch information and biological covariates.

## Project Structure

├── R/                                # R scripts for data processing and harmonization
│   ├── covariates.R                  # Handles covariate extraction and processing
│   ├── neuroComBat_harmonisation.R   # Implements NeuroCombat harmonization
│   └── subfeatures.R                 # Processes subfeatures for ROI-based analysis
├── visualisation_results/            # Directory for storing visualizations and analysis outputs
│   ├── harmonisation_results/        # Visual outputs of harmonized QSM data
│   ├── qsm_features_outlier_detection/ # Plots for detecting outliers in QSM features
│   └── statistical_results/          # Statistical analysis results, such as p-values and test statistics
├── README.md                         # Project documentation

## Instructions
1. Set up
- To set up the environment, make sure you have [Conda](https://docs.conda.io/en/latest/miniconda.html). Use the code below to install with all necessary dependencies:
```bash 
conda env create -f environment.yml
conda activate <environment_name>
```
- Install [QSMxT](https://qsmxt.github.io/QSMxT/) framework
- Install [neuroComBat R package](https://github.com/Jfortin1/neuroCombat_Rpackage)

2. Preprocessing
Run subfeatures.R to clean and organise qsm data. Run covariates.R to preprocess covariates data including biology and batch information.

3. Harmonisation and Visualisation
Use neuroComBat_harmonisation.R to apply neuroCombat harmonisation and use brain maps and statistical summaries results for visualisation.

## Issues and Improvements
### Completed
- Implemented neuroCombat harmonisation for QSM data, which successfully eliminated batch effects while preserving biological information.
- Applied some visualisations for regional QSM metrics.

### Limitation
The number of data might insufficient. Further research should include larger datasets and explore some more advanced approaches, such as machine learning- and deep learning-based harmonisation methods.

## References
- Stewart, A. W., Robinson, S. D., O’Brien, K., Jin, J., Widhalm, G., Hangel, G., ... & Bollmann, S. (2022). QSMxT: Robust masking and artifact reduction for quantitative susceptibility mapping. Magnetic resonance in medicine, 87(3), 1289-1300. https://doi.org/10.1002/mrm.29048
- Fortin, J. P., Cullen, N., Sheline, Y. I., Taylor, W. D., Aselcioglu, I., Cook, P. A., Adams, P., Cooper, C., Fava, M., McGrath, P. J., McInnis, M., Phillips, M. L., Trivedi, M. H., Weissman, M. M., & Shinohara, R. T. (2018). Harmonization of cortical thickness measurements across scanners and sites. Neuroimage, 167, 104-120. https://doi.org/10.1016/j.neuroimage.2017.11.024
- Jfortin1. neuroCombat_Rpackage. *GitHub* https://github.com/raoyongming/GFNet
- ggseg. *GitHub* https://github.com/ggseg/ggseg
- ggseg. Using geom_brain. https://ggseg.github.io/ggseg/articles/ggseg.html

