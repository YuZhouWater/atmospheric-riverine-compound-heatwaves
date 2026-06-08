# README
## Overview

This repository contains R scripts and example data for processing, analyzing, and visualizing atmospheric–riverine compound heatwaves across river basins. 

The scripts include data handling, riverine and atmospheric heatwave detection, compound-event identification, statistical tests, trend analysis, and figure generation for manuscript preparation.


## Code Structure

This code is organized into three main directories:
#### 1) **code/**, includes R scripts for compound heatwave detection, analysis, and plotting
#### 2) **data/**, includes example input datasets for running and testing the scripts
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
│   └── Fig1_trend_summary.csv
│
├── Fig2/
│   ├── Fig2a_map_CE.png
│   ├── Fig2a_map_US.png
│   ├── Fig2b.png
│   ├── Fig2c.png
│   └── Fig2d.png
│
├── Supplementary/
│   ├── ARCH_example_plot.png
│   ├── ARCH_sensitivity_plot.png
│   └── ARCH_sensitivity_summary.csv
│
└── output
```


## Important note on figure layout

The final multi-panel figures presented in the manuscript were assembled and refined manually using Adobe Illustrator, following the generation of individual panels by the code. As such, this repository reproduces all individual figure panels, not the exact final layout or composite panels as they appear in the manuscript.

In addition, due to differences in package versions and local desktop environments, the figures generated may exhibit minor aesthetic variations (e.g., font, color, legend) from those in the submitted manuscript. The underlying results and conclusions remain unchanged.
