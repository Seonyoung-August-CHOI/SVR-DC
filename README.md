# SVR-DC

This repository contains modular R functions and simulation code for evaluating SVR methods for cencored data.

## Structure
- **Modular functions**: `*.R` files for core components such as IPCW, kernel functions, and optimization wrappers.
- **Main simulations**:
  - Setting 1: Log-linear failure time, censoring independent
  - Setting 2: Nonlinear failure time, censoring independent
- **Evaluation metrics**:
  - NMAE (Normalized MAE)
  - C-index 

## Compared methods
- Cox PH
- SVCR
- IPCW (KM and PH)
- WCSVR (KM and PH) 
