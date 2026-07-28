# README
## Overview

This repository provides R scripts and example data for detecting, analyzing, and visualizing atmospheric heatwaves (AHWs), riverine heatwaves (RHWs), and atmospheric–riverine compound heatwaves (ARCHs).

The repository supports the main analyses used in the manuscript, including heatwave event detection(AHWs/RHWs), compound event(ARCHs) identification, trend analysis, sensitivity analysis, future projection analysis, and figure generation.


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
│   ├── ARCH_frequency_by_time_gap.csv
│   ├── Future_heatwave_duration.csv
│   ├── AHW_RHW_metric.csv
│   ├── Attribute_data.csv
│   ├── Example_40_rivers_DO.csv
│   ├── Example_40_rivers_Tmax.csv
│   ├── Example_40_rivers_WT.csv
│   ├── Observed_DO_796_rivers.csv
│   └── Observed_WT_796_rivers.csv

LICENSE
```


## Code description

`AHW_RHW_ARCH_event_detection.R`
Detects atmospheric heatwaves, riverine heatwaves, and atmospheric–riverine compound heatwaves.

`AHW_RHW_trend_analysis.R`
Calculates and visualizes trends in AHWs and RHWs characteristics.

`ARCH_trend_analysis.R`
Calculates and visualizes the trend in ARCH frequency.

`ARCH_sensitivity.R`
Calculates and visualizes ARCH sensitivity to different compound event time windows.

`ARCH_future_duration_and_proportion.R`
Calculates and visualizes future changes in heatwave duration and compound event proportions.


## Data description

Descriptions of the datasets provided with this repository are available in [`data/README.md`](data/README.md).


## Related resources

Other datasets required for the full analysis, including site lists, basin attributes, discharge data, and meteorological forcing data, are available from the WT–DO US–CE dataset repository:

* [WT–DO US–CE dataset](https://github.com/WeiZhiWater/WT-DO-US-CE-dataset)

The LSTM model used to reconstruct water temperature and dissolved oxygen time series is available from:

* [WT–DO US–CE LSTM](https://github.com/LiReactiveWater/WT-DO-US-CE-LSTM)


## Important note on figure layout

The final multi-panel figures presented in the manuscript were assembled and refined manually using Adobe Illustrator, following the generation of individual panels by the code. As such, this repository reproduces all individual figure panels, not the exact final layout or composite panels as they appear in the manuscript.

In addition, due to differences in package versions and local desktop environments, the figures generated may exhibit minor aesthetic variations (e.g., font, color, legend) from those in the submitted manuscript. The underlying results and conclusions remain unchanged.
