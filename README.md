# PSOBrainModeler

![version](https://img.shields.io/badge/version-0.5.2-blue.svg)

Particle Swarm Optimization-Based Hyperparameter Tuning for Support Vector Regression Models in Cerebral Autoregulation Analysis

---

## Description

This package offers a comprehensive toolkit for the analysis and modeling of biological signal data specific to individual patients. It facilitates the training of Support Vector Regression (SVR) models to represent and predict cerebral autoregulation phenomena. 

Features include:
- Utilizing blocked k-fold cross-validation for training.
- Hyperparameter optimization through Particle Swarm Optimization (PSO).
- Generation of various models including:
  - Finite Impulse Response (FIR)
  - Nonlinear Finite Impulse Response (NFIR)
  - AutoRegressive with eXogenous inputs (ARX)
  - Nonlinear AutoRegressive with eXogenous inputs (NARX)
- Both univariate and multivariate modeling.
- A scoring filter to evaluate the quality of the autoregulation response, especially when a patient is subjected to simulated pressure changes.

---

## Installation

```R
# devtools::install_github("benjajorquera/PSOBrainModeler")
```

---

## Dependencies

- dplyr
- signal
- stats
- e1071
- utils
- pso
- progress
- tseries
- magrittr

---

## Suggested Packages

- testthat

---

## License

MIT License. See [LICENSE](LICENSE) file for more details.

---

## Authors

- **Benjamin Jorquera** - *Primary Author & Maintainer* - [Email](mailto:benjamin.jorquera@usach.cl)
- **Jose Luis Jara** - [Email](mailto:jljara@usach.cl)

---

