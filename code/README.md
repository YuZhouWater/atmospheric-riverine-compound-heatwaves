# README
## Overview

This capsule contains the code and data required to demonstrate the core analytical workflow described in the submitted manuscript. To ensure efficient runtime and manageable data volume for peer review, here we prioritize the reproduction of Figures 1, 2, and 6.

The recommended starting point for peer review is: **code/00_run_first.R**, all required datasets are already included in the **data/** directory to ensure efficient execution.

## Important note on figure layout

The final multi-panel figures presented in the manuscript were assembled and refined manually using Adobe Illustrator, following the generation of individual panels by the code. As such, this capsule reproduces all individual figure panels, not the exact final layout or composite panels as they appear in the manuscript.

In addition, due to differences in package versions between the Code Ocean and local desktop environments, the figures generated here may exhibit minor aesthetic variations (e.g., font, color, legend) from those in the submitted manuscript. The underlying results and conclusions remain unchanged.

## Capsule Structure

This capsule is organized into three main directories:
#### 1) **code/**, includes R scripts for riverine heatwave detection, analysis, and plotting
#### 2) **data/**, includes required input and supplementary datasets to run the scripts 
#### 3) **results**/, include plots and output as follows


```text
results/
│
├── Fig1/
│   ├── Fig1_map_CE_duration.png
│   ├── Fig1_map_CE_frequency.png
│   ├── Fig1_map_CE_intensity.png
│   ├── Fig1_map_US_duration.png
│   ├── Fig1_map_US_frequency.png
│   ├── Fig1_map_US_intensity.png
│   ├── Fig1_trend_plot.png
│   ├── Fig1_trend_summary.csv
│
├── Fig2/
│   ├── Fig2a_map_CE.png
│   ├── Fig2a_map_US.png
│   ├── Fig2b.png
│   ├── Fig2c.png
│   └── Fig2d.png
│
├── Fig6/
│   ├── Fig6a_ARCH_main.png
│   └── Fig6b_ARCH_zoom.png
│
├── Supplementary/
│   ├── ARCH_sensitivity_plot.png
│   └── ARCH_sensitivity_summary.csv
│
└── output
```




