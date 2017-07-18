# NetBenefitCurve

Net benefit curves (also called decision curves) are a useful tool to evaluate the population impact of adopting a risk prediction instrument into clinical practice. Given one or more instruments (risk models) that predict the probability of a binary outcome, this package calculates and plots decision curves, which display estimates of the standardized net benefit by the probability threshold used to categorize observations as 'high risk.' Curves can be estimated using data from an observational cohort, or from case-control studies when an estimate of the population outcome prevalence is available.  Version 1.4 of the package provides an alternative framing of the decision problem for situations where treatment is the standard-of-care and a risk model might be used in order for low-risk patients (i.e., patients below some risk threshold) to opt out of treatment.

Confidence intervals calculated using the bootstrap can be displayed and a wrapper function to calculate cross-validated curves using k-fold cross-validation is also provided. 

Key functions are: 

- `decision_curve`: Estimate (standardized) net benefit curves with bootstrap confidence intervals. 

- `plot_decision_curve`: Plot a decision curve or multiple curves.

- `plot_clinical_impact` and `plot_roc_components`: Alternative plots for the output of `decision_curve`. See help files or tutorial for more info. 

- `cv_decision_curve`: Calculate k-fold cross-validated estimates of decision curves. 

## Installation

The easiest way to get the package is directly from CRAN:

```r
install.packages("NetBenefitCurve")
```

You may also download the current version of the package here:

[https://github.com/mdbrown/NetBenefit/releases](https://github.com/mdbrown/NetBenefit/releases)

navigate to the source package and use 

```r
install.packages("../NetBenefitCurve_1.4.tar.gz", 
                  repos = NULL, 
                  type = "source")
```

or install the package directly from github using devtools. 
 
```r
## install.packages("devtools")
library(devtools)
install_github("mdbrown/NetBenefitCurve")
```


click [here](http://mdbrown.github.io/NetBenefitCurve/) for a tutorial to get you started. 
