# askmyGP-descriptive-analysis

#### Project Status: On-going
  [![R build status](https://github.com/THF-evaluative-analytics/THFstyle/workflows/R-CMD-check/badge.svg)](https://github.com/THF-evaluative-analytics/askmyGP-descriptive-analysis/actions)
  
## About this project

The Improvement Analytics Unit (IAU) has undertaken an evaluation of the impact of different models of remote consultation and online triage on patient access, clinical outcomes, staff workload and retention, and secondary care outcomes. There are many providers of digital technology to support remote consultation and online triage in primary care. For this analysis, we have been working with askmyGP. 

askmyGP provides online triage tools which clinicians and staff can use to prioritise and deliver care either by online message, a telephone, video or face-to-face consultation, or a home visit. Patients or carers use askmyGP to contact their practice either online via a link from the practice website, or by telephone to the practice directly.  Patients can state their preference for a face-to-face, telephone or video consultation. This triaging approach aims to provide patients appropriate access to general practice, whilst improving efficiency and access. 

Here we present an initial descriptive analysis derived from data covering a subset of 51 practices in England that were using askmyGP for all consultation requests prior to the outbreak of COVID-19 in March 2020. This analysis supported the publication of a long chart published by the Health Foundation. 

## Requirements

These scripts were written in RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed to run the scripts:

tidyverse,
data.table,
lubridate,
glue,
here,
forcats,
janitor,
flextable,
RColorBrewer

## Summary of R codes

The 'R' folder contains:

0_preamble.r: Loads R libraries and sets up general parameters for the analysis and is then sourced from subsequent files. 

1_read_in_data.r: Reads in the files sent by askmyGP and the GP reference file (which contains one row for each GP practice each month with relevant publicly available data such as % males, %age groups, deprivation score, rurality scores, QOF scores etc). Each row of the askmygp data represents a single request for a consultation from a patient. In the end it writes out general datasets that will be used later. 

2_select_askmyGP_practices.r: Checks the number of requests for each practice each month and categorises the practices according to the volume of requests relative to the patient list size. In our analysis we only look at the practices which had high volumes, operating at full capacity. In the end it writes out sets of GP practices based on groups belonging into. 

3_create_final_datasets.r: Contains all data manipulation steps and creation of new fields that will be used later on for the descriptive analysis. It also links the fields from the GP reference file to the requests and feedback datasets. In the end it writes outs all clean and tidy datasets for later use.

4_descriptive_analysis_tables.r: Creates all tables and summary descriptive statistics that we published in our report. This includes descriptive information on the askmyGP GP practices selected and crude rates tables for specific variables and groupings.

5_descriptive_analysis_plots.r: Creates all plots and the data used to create them that we published in our report. This includes plots of consultation rates by month, week, age groups and consultation type.

## Data source

askmyGP data on patients requests and feedback between August 2018 and July 2020 linked to publicly available GP characteristics data (NHS Digital. Number of patients registered at a GP practice. 2011-2019, Office for National Statistics. Census 2011, Department for Communities and Local Government. English indices of deprivation.  2015)

## Authors

This analysis was conducted by:

Geraldine Clarke [Twitter](https://twitter.com/GeraldineCTHF)

Paris Pariza [Twitter](https://twitter.com/ParizaParis) - [Github](https://github.com/Ppariz)

## License

This project is licensed under the [MIT License](LICENSE.md).

