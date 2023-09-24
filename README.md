# PSOBrainModeler

![version](https://img.shields.io/badge/version-0.1.0-blue.svg)

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

\`\`\`R
# You can provide instructions on how to install the package from its source or any repository.
# For example, if it's on CRAN:
# install.packages("PSOBrainModeler")

# If it's on GitHub and you're using devtools:
# devtools::install_github("YourGitHubUsername/PSOBrainModeler")
\`\`\`

---

## Dependencies

- dplyr
- signal
- stats
- e1071
- utils
- pso

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

