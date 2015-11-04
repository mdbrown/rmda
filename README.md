# DecisionCurve

Decision curves are a useful tool to evaluate the population impact of adopting a risk prediction instrument into clinical practice. Given one or more instruments (risk models) that predict the probability of a binary outcome, this package calculates and plots decision curves, which display estimates of the standardized net benefit by the probabilty threshold used to categorize observations as 'high risk.'  Bootstrap confidence intervals are displayed as well. This package is a companion to the manuscript '(put ref here.)'.

## Installation

After downloading the package from here:

[https://github.com/mdbrown/DecisionCurve/releases](https://github.com/mdbrown/DecisionCurve/releases)

navigate to the source package and use 


```{r, eval = FALSE}
install.packages("../DecisionCurve_0.3.tar.gz", repos = NULL, type = "source")
```

or install the package directly from github. 
 
```{r, eval = FALSE}
## install.packages("devtools")
library(devtools)
install_github("mdbrown/DecisionCurve")
```

click [here](http://rpubs.com/mdbrown/DecisionCurveTutorial) for a quick tutorial to get you started. 
