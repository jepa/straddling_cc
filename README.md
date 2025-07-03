# straddling_stocks_cc

This repository supports the publication *Climate change drives shifts in straddling fish stocks in the world’s ocean* Currently *in press*

**Authors**: Juliano Palacios-Abrantes*1, Bianca S. Santos2, Thomas L. Frölicher3,4, Gabriel Reygondeau5, U. Rashid Sumaila1,6, Colette C. C. Wabnitz1,7 & William W. L. Cheung1

*Corresponding author: Juliano Palacios-Abrantes, j.palacios@oceans.ubc.ca

Paper publication **TDB**

Data accompaning the code [![DOI](https://zenodo.org/badge/483446639.svg)](https://doi.org/10.5281/zenodo.11496334)

# Project Goal:

Identify straddles between the high seas and EEZs and investigate the effects of climate-induced distribution shifts on the sharing of straddling stocks.

i) which stocks straddle between the high seas and global EEZs 

ii) weather climate change will affect the proportion of the distribution between EEZs and the high-seas.

# Files and folders organization:

In this repository you will find all of the code related to the manuscript 

## Repository structure

```
straddling_cc/                    
├── functions/                    # Custom R functions used throughout the project
│   ├── circle_figures_fx         # Function to create all circle figures
│   ├── get_results.R        
│   ├── get_spp_dist.R            
|   ├── rfmo_taxon_proportion.R
|   ├── stradd_index.R
│   └── taxon_proportion.R        
├── results/                      # Output directory for processed data, figures, and tables
│   ├── figures/                  # Figures and plots generated from analyses  
│   ├── supplements/              # Figures and plots generated for supplements      
│   └── tables/                   # Tables summarizing results
├── scripts/                      # R scripts for executing analyses
│   ├── main_analysis.Rmd         # Rmarkdown file containing all the analysis
├── .gitignore                    # Specifies which files/folders Git should ignore
├── LICENSE                       # License file (e.g., MIT, CC BY-NC 4.0)
├── MigrantFish.Rproj            # RStudio project file
└── README.md                    # Project overview and instructions
```

# Data
The raw data used in this analysis comes from different sources including OBIS, GBIF, The Sea Around Us, and FishBase, among others. These can be sourced directly from their respectively webpages and/or the references provided on the text. The data containing the identification of transboundary species per eez and fishing nation and the catch trend of each taxon per eez can be found here. Please feel free to email me to discuss anything related to data sharing or code reproducibility.


