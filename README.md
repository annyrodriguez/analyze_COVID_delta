---
title: "Analysing the COVID-19 Delta Variant Data"
author: "Anny Rodriguez and Gabriel Odom"
date: 2021-08-03
---
  

## Introduction
We have wrangled, cleaned, and extracted information from the CDC's [open data repository](https://healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9) related to the Delta variant of COVID-19. Our aim is to better display the state of the pandemic in the South Florida region of Miami-Dade, Broward, and Palm Beach counties.


## Our Processs
In order to replicate our work, here are the following steps:

1. Go to <https://healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9> and download the `.xlsx` files for the dates of interest. We check for new data files daily. 
2. Save the selected data files to `data_CDC_raw/` (do not change the file names; they should all be in the form "Community_Profile_Report_[YYYYMMDD]_Public.xlsx").
3. Wrangle all the data files in the `data_CDC_raw/` directory: the most recent script (as of 12 January 2022) is `scripts/wrangle_CDC_20220112.R`. Write the first "draft" of the wrangled data to `data_clean/` with this script.
4. After wrangling the data (the first pass), we run the plotting script `scripts/all_plots_20210805.R`. This script includes additional figure-specific data wrangling steps.

