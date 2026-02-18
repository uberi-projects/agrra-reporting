# AGRRA Reporting  <img src='www/images/UBERI_Logo.png' align="right" height="70" />

![status](https://img.shields.io/badge/status-continuous%20dev.-blue)

This repository stores the code for analysis of AGRRA reporting activities in Belize done in whole or part by UB-ERI. Specific reporting instances/projects are stored in their own dedicated folders.

## 2026 - Atolls Report

This project analyzes AGRRA data for Turneffe Atoll and Lighthouse Atoll, producing analyses and figures for reporting. To run this project, drop your copy of Lighthouse Atoll AGRRA data into the `data_deposit` subfolder. Turneffe data will be automatically downloaded from Dryad, so there is no need to upload this data as well. Open `project_run.r`, and run the script. Results will be located in the `outputs` subfolder.

#### Files
- `packages_load.r` - installs (if necessary) and attaches necessary R packages
- `data_attach.r`
  1. Downloads TAMR AGRRA data from Dryad and attaches it
  2. Attaches Lighthouse AGRRA data from `data_deposit`
  3. Attaches reference data from `data_provided`
- `data_analysis.r` - Analyzes the AGRRA data
- `results_export.r` - Exports the results of the analysis into the `outputs` folder

#### External
The external repository `uberi-projects/RHI-calculations-Turneffe` on GitHub is source within this project to access the `functions_define.r` script functions