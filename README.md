# README
## Overview

This repository provides R scripts and example data for detecting, analyzing, and visualizing atmospheric heatwaves (AHWs), riverine heatwaves (RHWs), and atmospheric–riverine compound heatwaves (ARCHs).

The repository supports the main analyses used in the manuscript, including heatwave event detection, compound event identification, trend analysis, sensitivity analysis, future projection analysis, and figure generation.


## Structure

This code is organized into two main directories:
#### 1) **code/**, includes R scripts for compound heatwave detection, analysis, and plotting
#### 2) **data/**, includes example input datasets for running and testing the scripts


```text
code/
│   ├── AHW_RHW_ARCH_event_detection.R
│   ├── AHW_RHW_trend_analysis.R
│   ├── ARCH_future_duration_and_proportion.R
│   ├── ARCH_sensitivity.R
│   └── ARCH_trend_analysis.R

data/
│   ├── ARCH_frequency_by_time_gap/
│   ├── future_projection/
│   ├── AHW-RHW-metric.csv
│   ├── ARCH_freq_by_elevation.csv
│   ├── attribute_data.csv
│   ├── example_40_rivers_DO.csv
│   ├── example_40_rivers_Tmax.csv
│   ├── example_40_rivers_WT.csv
│   ├── observed_DO_796_rivers.csv
│   └── observed_WT_796_rivers.csv

LICENSE
```


## Code description

`AHW_RHW_ARCH_event_detection.R`
Detects atmospheric heatwaves, riverine heatwaves, and atmospheric–riverine compound heatwaves.

`AHW_RHW_trend_analysis.R`
Calculates and visualizes trends in AHW and RHW characteristics.

`ARCH_trend_analysis.R`
Analyzes how ARCH frequency trends vary across elevation classes.

`ARCH_sensitivity.R`
Tests the sensitivity of ARCH identification to different compound-event time windows.

`ARCH_future_duration_and_proportion.R`
Analyzes future changes in heatwave duration and calculates the proportion of AHW/RHW days occurring as compound events.

## Data description

`ARCH_frequency_by_time_gap/`
Data used for sensitivity analysis under different compound event time windows.

`future_projection/`
Future projection data used to analyze changes in heatwave duration and compound event proportions.

`AHW-RHW-metric.csv`
Summary metrics of atmospheric and riverine heatwaves.

`ARCH_freq_by_elevation.csv`
ARCH frequency summarized by elevation classes.

`example_40_rivers_Tmax.csv`
Example Tmax data used for AHWs detection and ARCHs detection.

`example_40_rivers_WT.csv`
Example WT data used for RHWs and ARCHs detection.

`example_40_rivers_DO.csv`
Example dissolved oxygen data.

`attribute_data.csv`
Static basin attributes used for the LSTM model.

`observed_DO_796_rivers.csv`
Observed DO data for LSTM model training and evaluation.

`observed_WT_796_rivers.csv`
Observed WT data for LSTM model training and evaluation.


## Related resources

Other datasets required for the full analysis, including site lists, basin attributes, discharge data and meteorological forcing data, are available from the WT–DO US–CE dataset repository:

WT–DO US–CE dataset
The LSTM model used to reconstruct water temperature and dissolved oxygen time series is available from:

WT–DO US–CE LSTM


## Important note on figure layout

The final multi-panel figures presented in the manuscript were assembled and refined manually using Adobe Illustrator, following the generation of individual panels by the code. As such, this repository reproduces all individual figure panels, not the exact final layout or composite panels as they appear in the manuscript.

In addition, due to differences in package versions and local desktop environments, the figures generated may exhibit minor aesthetic variations (e.g., font, color, legend) from those in the submitted manuscript. The underlying results and conclusions remain unchanged.
