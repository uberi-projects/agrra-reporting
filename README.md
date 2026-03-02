# AGRRA Reporting  <img src='www/images/UBERI_Logo.png' align="right" height="70" />

![status](https://img.shields.io/badge/status-continuous%20dev.-blue)

This repository stores the code for analysis of AGRRA reporting activities in Belize done in whole or part by UB-ERI. Specific reporting instances/projects are stored in their own dedicated folders.

## 2026 - Atolls Report

This project analyzes AGRRA data for Turneffe Atoll and Lighthouse Atoll, producing analyses and figures for reporting. To run this project, place your copy of Lighthouse Atoll AGRRA data into the `data_deposit` subfolder. Turneffe data will be automatically downloaded from Dryad, so there is no need to upload this data as well. Open `project_run.r`, and run the script. Results will be located in the `outputs` subfolder. 

**Required AGRRA files for BAS include:**
  1. BAS AGRRA Benthic 2025_Clean.xlsx
  2. BAS AGRRA Coral Data 2025_Cleaned.xlsx
  3. BAS AGRRA Fish Data 2025_clean.xlsx

#### Files
- `packages_load.r` - installs (if necessary) and attaches necessary R packages
- `data_attach.r`
  1. Downloads TAMR AGRRA data from Dryad and attaches it
  2. Attaches Lighthouse AGRRA data from `data_deposit`
  3. Attaches reference data from `data_provided`
- `data_merge.r` - Harmonizes the data between both atolls in master dfs for each methodology and merges in reference data
- `data_analysis.r` - Analyzes the AGRRA data, calculating indicators and key observations for both atolls
- `results_export.r` - Exports the results of the analysis into the `outputs` folder

#### External
The external repository `uberi-projects/RHI-calculations-Turneffe` on GitHub is sourced within this project to access the `functions_define.r` script functions

#### AI Disclaimer
All parts of this project were conceptualized and planned by the UB-ERI team. To support implementation, Claude Code (model Sonnet 4.6) was used to review code and outputs, and check documentation as prompted by the team. The UB-ERI team retains full responsibility for this work, including any mistakes or oversights, and does not defer responsibility to the AI model.