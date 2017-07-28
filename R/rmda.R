#' rmda: Risk Model Decision Analysis
#'
#' The package `rmda` provides tools to evaluate the population impact of adopting a risk prediction instrument into clinical practice.  Given one or more instruments (risk models) that predict the probability of a binary outcome, we provide functions to calculate and display decision curves and other figures that help communicate the benefits and costs associated with using a risk model for decision making. 
#' 
#' Decision curves display estimates of the standardized net benefit over a range of probability thresholds used to categorize observations as 'high risk.' Curves can be estimated using data from an observational cohort, or from case-control studies when an estimate of the population outcome prevalence is available.  Version 1.4 of the package provides an alternative framing of the decision problem for situations where treatment is the standard-of-care and a risk model might be used in order for low-risk patients (i.e., patients below some risk threshold) to opt out of treatment.
#' 
#' Confidence intervals calculated using the bootstrap can be displayed and a wrapper function to calculate cross-validated curves using k-fold cross-validation is also provided. 
#'
#' Functions in this package include:
#'  \code{\link{decision_curve}}, 
#'  \code{\link{summary.decision_curve}},
#'  \code{\link{plot_decision_curve}},
#'  \code{\link{plot_clinical_impact}},
#'  \code{\link{plot_roc_components}},
#'  \code{\link{cv_decision_curve}} and
#'  \code{\link{Add_CostBenefit_Axis}}.
#'
#'
"_PACKAGE"
#> [1] "_PACKAGE"
