# DecisionCurve

Decision curves are a useful tool to evaluate the population impact of adopting a risk prediction instrument into clinical practice. Given one or more instruments (risk models) that predict the probability of a binary outcome, this package calculates and plots decision curves, which display estimates of the standardized net benefit by the probability threshold used to categorize observations as 'high risk.' Curves can be estimated using data from an observational cohort, or from case-control studies when an estimate of the population outcome prevalence is available.

Confidence intervals calculated using the bootstrap can be displayed and a wrapper function to calculate cross-validated curves using k-fold cross-validation is also provided. 


## Installation

After downloading the package from here:

[https://github.com/mdbrown/DecisionCurve/releases](https://github.com/mdbrown/DecisionCurve/releases)

navigate to the source package and use 


```{r, eval = FALSE}
install.packages("../DecisionCurve_1.1.tar.gz", repos = NULL, type = "source")
```

or install the package directly from github. 
 
```{r, eval = FALSE}
## install.packages("devtools")
library(devtools)
install_github("mdbrown/DecisionCurve")
```

click [here](http://mdbrown.github.io/DecisionCurve/) for a tutorial to get you started. 
