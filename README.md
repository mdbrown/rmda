# `rmda`: Risk Model Decision Analysis

The package `rmda` (risk model decision analysis) provides tools to evaluate the value of using a risk prediction instrument to decide treatment or intervention (versus no treatment or intervention).  Given one or more risk prediction instruments (risk models) that estimate the probability of a binary outcome, `rmda` provides functions to estimate and display decision curves and other figures that help assess the population impact of using a risk model for clinical decision making.   Here, “population” refers to the relevant patient population.

Decision curves display estimates of the (standardized) net benefit over a range of probability thresholds used to categorize observations as 'high risk.' The curves help evaluate a treatment policy that recommends treatment for patients who are estimated to be ‘high risk’ by comparing the population impact of a risk-based policy to “treat all” and “treat none” intervention policies.  Curves can be estimated using data from a prospective cohort.  In addition, `rmda` can estimate decision curves using data from a case-control study if an estimate of the population outcome prevalence is available.  Version 1.4 of the package provides an alternative framing of the decision problem for situations where treatment is the standard-of-care and a risk model might be used to recommend that low-risk patients (i.e., patients below some risk threshold) opt out of treatment.

Confidence intervals calculated using the bootstrap can be computed and displayed. A wrapper function to calculate cross-validated curves using k-fold cross-validation is also provided. 

Key functions are: 

- `decision_curve`: Estimate (standardized) net benefit curves with bootstrap confidence intervals. 

- `plot_decision_curve`: Plot a decision curve or multiple curves.

- `plot_clinical_impact` and `plot_roc_components`: Alternative plots for the output of `decision_curve` showing measures of clinical impact or the components of the ROC curve (true/false positive rates) across a range of risk thresholds. See help files or tutorial for more info. 

- `cv_decision_curve`: Calculate k-fold cross-validated estimates of a decision curve and its components. 


## Installation

The easiest way to get the package is directly from CRAN:

```r
install.packages("rmda")
```


or install the package directly from github using devtools. 
 
```r
## install.packages("devtools")
library(devtools)
install_github("mdbrown/rmda")
```
You may also download the current version of the package here:

[https://github.com/mdbrown/rmda/releases](https://github.com/mdbrown/rmda/releases)

navigate to the source package and use 

```r
install.packages("../rmda_1.4.tar.gz", 
                  repos = NULL, 
                  type = "source")
```

## Tutorial 

Click [here](http://mdbrown.github.io/rmda/) for a tutorial to get you started. 
