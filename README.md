[newRR3-Rpackage]: https://github.com/DrylandEcology/newRR3-Rpackage
[newRR3-analysis]: https://github.com/DrylandEcology/newRR3-analysis



# R package: Analysis Framework for the `"newRR"` Project
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8310206.svg)](https://doi.org/10.5281/zenodo.8310206)


Contact: Daniel R Schlaepfer [ORCiD](https://orcid.org/0000-0001-9973-2065)


This repository contains code developed for the manuscript

**Declining ecological resilience and resistance indicators under climate change
in the sagebrush region, United States**

by Daniel R Schlaepfer, Jeanne C Chambers, Alexandra K Urza,
Brice B Hanberry, Jessi L Brown, David I Board, Steven B Campbell,
Karen J Clause, Brice B Hanberry, Alexandra K Urza, Michele R Crist,
and John B Bradford


<br>

Code to replicate analyses by Schlaepfer et al. requires
  * the R package `"newRR3"`
    (this repository)
  * R scripts utilizing functionality from `"newRR3"`
    (available from the repository [newRR3-analysis][])


<br>

The objective of this study was to understand where and why climate change
will alter the distribution of ecological resilience and resistance
in the sagebrush biome throughout the 21st century.
To accomplish this, the study pursued five specific objectives:
  1. Estimate the new R&R indicators under future climate conditions and
     quantified changes from historical conditions.
  1. Develop a continuous R&R index that integrates probability information
     from the underlying predictive R&R models.
  1. Assess (i) the robustness of projected changes in R&R to
     uncertainty in future climate conditions;
     (ii) certainty of the predictive R&R model; and
     (iii) novelty among predictors.
  1. Identify causes of projected changes in R&R.
  1. Examined how future projected R&R indicators relate to
     recently-defined geographic patterns of ecological integrity
     across the sagebrush biome.


<br>
<a name="installation"></a>

## Installation

The R package `"newRR3"` (and its dependencies) can be installed from github
  ```{r}
  remotes::install_github("DrylandEcology/newRR3-Rpackage")
  ```
