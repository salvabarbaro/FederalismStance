# FederalismStance

This repository contains replication files for the paper *"What explains German Länder governments' stances towards federalism?"* by **Salvatore Barbaro and Julia M. Rode**. Publius. The Journal of Federalism (2025), DOI 10.1093/publius/pjae050 

## Table of Contents

1. [Data](#data)
2. [Scripts](#scripts)
3. [Remarks](#remarks)
4. [Data Accessibility](#data-accessibility)
5. [Dataset List](#dataset-list)
6. [Software Requirements](#software-requirements)
7. [Controlled Randomness](#controlled-randomness)
8. [Reproduction Time](#reproduction-time)
9. [Code Description](#code-description)
10. [Instructions for Replicators](#instructions-for-replicators)

---

## Data

### Included Files
- **`microdf.RData`**: Contains all microdata associated with the assessment of states' stances. (R-specific file)
- **`LFAseit1950.csv`**: Fiscal equalization scheme data since 1950. Source: Federal Ministry of Finance, Federal Statistical Office.
- **`Fed_ind_Fleiss.RData` & `Att_t_fed.RData`**: 
  - `Att_t_fed.RData`: Data on statements in coalition agreements.
  - `Fed_ind_Fleiss.RData`: Statements of Land incumbents used in a separate research project (not directly related to this project).

## Scripts

### Overview
- **`FactorAnalysis.R`**: Performs statistical evaluations related to states' stances. Includes comments explaining each step.
- **`BWpics.R`**: Generates all figures in black and white.
- **`Script02.R`**: Analyzes survey data for the state of Baden-Württemberg (BW) using GESIS datasets. Includes supplementary material analysis.
- **`FleissKappa.R`**: Calculates Fleiss-Kappa values.

## Remarks

- The scripts have been tested on both Linux and Windows operating systems, with no reported issues.
- Execution may vary slightly depending on the system setup and software version.

---

## Data Accessibility

- **Provided Data**: Self-conducted data and data from the Federal Statistical Office are included.
- **External Data**: Access to GESIS data must be obtained through [GESIS](https://www.gesis.org).

---

## Dataset List

See the [Data](#data) section for a list of included datasets.

---

## Software Requirements

- **R Version**: 4.1.2 ("Bird Hippie") or higher.
- **Operating System**: Tested primarily on Debian Linux.

---

## Controlled Randomness

- Controlled randomness is generally not applicable.
- Exception: The `FactorAnalysis.R` script uses a bootstrap procedure with a fixed seed for robustness checks.

---

## Reproduction Time

- **Without Bootstrap**: ~2 minutes.
- **With Bootstrap**: ~3 minutes.
- **Optimization Tip**: Use `gisco_get_nuts()` with `resolution = 20` instead of `resolution = 03` for faster processing.

---

## Code Description

- Inline comments within each script provide detailed explanations.

---

## Instructions for Replicators

No special instructions are needed. Please ensure you have the required R version and dependencies installed.

---
