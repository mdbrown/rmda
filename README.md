# DecisionCurve

Decision curves are a useful tool to evaluate the population impact of adopting a risk prediction instrument into clinical practice. Given one or more instruments (risk models) that predict the probability of a binary outcome, this package calculates and plots decision curves, which display estimates of the standardized net benefit by the probability threshold used to categorize observations as 'high risk.' Curves can be estimated using data from an observational cohort, or from case-control studies when an estimate of the population outcome prevalence is available.

Confidence intervals calculated using the bootstrap can be displayed and a wrapper function to calculate cross-validated curves using k-fold cross-validation is also provided. 

Key functions are: 

- `decision_curve`: Estimate (standardized) net benefit curves with bootstrap confidence intervals. 

- `plot_decision_curve`: Plot a decision curve or multiple curves.

- `plot_clinical_impact` and `plot_roc_components`: Alternative plots for the output of `decision_curve`. See help files or tutorial for more info. 

- `cv_decision_curve`: Calculate k-fold cross-validated estimates of decision curves. 

## Installation

The easiest way to get the package is directly from CRAN:

```r
install.packages("DecisionCurve")
```

You may also download the current version of the package here:

[https://github.com/mdbrown/DecisionCurve/releases](https://github.com/mdbrown/DecisionCurve/releases)

navigate to the source package and use 

```r
install.packages("../DecisionCurve_1.1.tar.gz", 
                  repos = NULL, 
                  type = "source")
```

or install the package directly from github using devtools. 
 
```r
## install.packages("devtools")
library(devtools)
install_github("mdbrown/DecisionCurve")
```


click [here](http://mdbrown.github.io/DecisionCurve/) for a tutorial to get you started. 
