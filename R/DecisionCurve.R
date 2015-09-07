#' Plot decision curves
#'
#'Decision curves are a useful tool to evaluate the population impact of adopting a risk prediction instrument into clinical practice. Given one or more instruments (risk models) that predict the probability of a binary outcome, this package calculates and plots decision curves, which display estimates of the standardized net benefit by the probabilty threshold used to categorize observations as 'high risk.'  Bootstrap confidence intervals are displayed as well. This package is a companion to the manuscript 'Kerr. et. al (put ref here.)'.
#'
#' @param data data.frame containing outcome and predictors. Missing data on any of the predictors will cause the entire observation to be removed.
#' @param outcome Name of outcome of interest found in data. Within 'data', the outcome variable must be a numeric vector of 0's and 1's.
#' @param predictors Vector of names for risk predictors to calculate decision curves. 'data' must have columns corresponding to the names provided consisting of predicted risks Pr(outcome = 1 | predictor). Risks must fall between 0 and 1.
#' @param thresholds Numeric vector of high risk thresholds to use when plotting and calculating net benefit values.
#' @param standardize logical (default TRUE) indicating whether to use the standardized net benefit (NB/disease prevalence) or not.
#' @param confidence.intervals Numeric (default 0.95 for 95\% confidence bands) level of bootstrap confidence intervals to plot. Set as NA or 'none' to remove confidence intervals. See details for more information.
#' @param bootstraps Number of bootstrap replicates to use to calculate confidence intervals (default 500).
#' @param cost.benefit.axis logical (default TRUE) indicating whether to print an additional x-axis showing relative cost:benefit ratios in addition to risk thresholds.
#' @param n.cost.benefits number of cost:benefit ratios to print if cost.benefit.axis = TRUE (default n.cost.benefit = 6).
#' @param cost.benefits Character vector of the form c("c1:b1", "c2:b2", ..., "cn:bn") with integers ci, bi corresponding to specific cost:benefit ratios to print.
#' @param legend.position One of "topright" (default), "top", "topleft", "left", "bottomleft", "bottom", "bottomright", "right", or "none" indicating where the legend should be printed.
#' @param col vector of color names to be used in plotting corresponding to the 'predictors' given. Default colors will be chosen from rainbow(..., v = .8). See details for more information on plot parameters.
#' @param lty vector of linetypes to plot corresponding to 'predictors'.
#' @param lwd vector of linewidths to plot corresponding to 'predictors'.
#' @param xlim vector giving c(min, max) of x-axis. Defaults to c(min(thresholds), max(thresholds)).
#' @param ylim vector giving c(min, max) of y-axis.
#' @param xlab label of main x-axis.
#' @param ylab label of y-axis.
#' @param cost.benefit.xlab label of cost:benefit ratio axis.
#' @param plot Logical (default TRUE) indicating whether to plot decision curves.
#' @param ... other options directly send to plot()
#'
#' @details  Confidence intervals for (standardized) net benefit are calculated pointwise at each risk threshold. Bootstrap sampling is done without stratifying on outcome, so disease prevalence varies within bootstrap samples. Note that the confidence intervals calculated assume the risk prediction tool
#' @return List with components
#' \itemize{
#'   \item plot.data: a list with further data.frame components 'estimates', 'ci_lower', and 'ci_upper' showing point estimates of (standardized) net benefits, and lower and upper ci's, respectively.
#'   \item derived.data: A data frame in long form showing the following for each predictor and each 'threshold', 'FPF':false positive fraction, 'TPF': true positive fraction, 'NB': net benefit, 'sNB': standardized net benefit, 'rho': outcome prevalence, 'predictor': name of predictor, 'x_lower', 'x_upper': the lower and upper confidence bands for NB or sNB.
#'   \item standardized: Whether standardized net benefit or net benefit is returned.
#'   \item call: matched function call.
#' }
#'
#' @seealso \code{\link{summary.DecisionCurve}},  \code{\link{Add_CostBenefit_Axis}}
#' @examples
#'#helper function
#' expit <- function(xx) exp(xx)/ (1+exp(xx))
#'
#'#load simulated data
#'data(dcaData)
#'# Assume we have access to previously published models
#'# (or models built using a training set)
#'# that we can use to predict the risk of cancer.
#'
#'# Basic model using demographic variables: Age, Female, Smokes.
#'dcaData$BasicModel <- with(dcaData, expit(-7.3 + 0.18*Age - 0.08*Female + 0.80*Smokes ) )
#'
#'# Model using demographic + markers : Age, Female, Smokes, Marker1 and Marker2.
#'dcaData$FullModel <- with(dcaData, expit(-10.5 + 0.22*Age  - 0.01*Female + 0.91*Smokes + 2.03*Marker1 - 1.56*Marker2))
#'
#'#use DecisionCurve defaults (set bootstraps = 25 here to reduce computation time).
#'DecisionCurve(dcaData,
#'             outcome = "Cancer", predictors = c("BasicModel", "FullModel"),
#'              bootstraps = 25)
#'
#' @export

DecisionCurve <- function(data,
                          outcome,
                          predictors,
                          thresholds = seq(0, 1, by = .01),
                          standardize = TRUE,
                          confidence.intervals = 0.95,
                          bootstraps = 500,
                          cost.benefit.axis = TRUE,
                          n.cost.benefits = 6,
                          cost.benefits,
                          legend.position = "topright",
                          col,
                          lty, lwd = 2,
                          xlim, ylim,
                          xlab = "Risk Threshold", ylab,
                          cost.benefit.xlab = "Cost:Benefit Ratio",
                          plot = TRUE,
                          ...){
  call <- match.call()

  #check that inputs are ok, return complete cases
  cc <- check_DC_Inputs(data = data,
                  outcome = outcome,
                  predictors = predictors,
                  thresholds = thresholds,
                  standardize = standardize,
                  confidence.intervals = confidence.intervals,
                  bootstraps = bootstraps,
                  legend.position = legend.position,
                  cost.benefit.axis = cost.benefit.axis,
                  cost.benefits = cost.benefits,
                  n.cost.benefits = n.cost.benefits)

  data <- data[cc, c(outcome, predictors)]

  legend.position <- legend.position[1]
  n.cost.benefits <- n.cost.benefits[1]
  standardize <- standardize[1]


  #calculate curves
  dc.data <- get_DecisionCurve(data, outcome, predictors,
                               threshold = thresholds,
                               standardize = standardize,
                               confidence.intervals = confidence.intervals,
                               bootstraps = bootstraps)


  #set some defaults if needed
  if(missing(xlim)) xlim = range(thresholds)

  if(missing(lty)) lty = rep(1, length(predictors) + 2)
  if(length(lty) ==1) lty = rep(lty, length(predictors) + 2)
  if(length(lty) == length(predictors)) lty = c(lty, 1, 1)

  if(missing(col)) col  = c(rainbow(length(predictors), v = .8), "grey66", "black")
  if(length(col) == length(predictors)) col <- c(col, "grey66", "black")

  if(missing(lwd)) lwd = 2
  if(length(lwd) ==1) lwd <- rep(lwd, length(predictors))
  if(length(lwd) == length(predictors)) lwd = c(lwd, 1, 1)

  if(missing(ylab)) ylab = ifelse(standardize, "Standardized Net Benefit", "Net Benefit")

  #plot the curves
  if(plot){
  plot_DecisionCurve(xx = dc.data,
                     predictors = predictors,
                     standardize = standardize,
                     confidence.intervals,
                     cost.benefit.axis = cost.benefit.axis,
                     cost.benefits = cost.benefits,
                     n.cost.benefits = n.cost.benefits,
                     cost.benefit.xlab = cost.benefit.xlab,
                     xlab = xlab, ylab = ylab,
                     col = col,
                     lty = lty, lwd = lwd,
                     xlim = xlim, ylim = ylim,
                     legend.position = legend.position, ...)
  }
  #return list of elements
  xx.wide <- cast(dc.data, threshold+cost.benefit.ratio~predictor, value = ifelse(standardize, "sNB", "NB"))
 if(is.numeric(confidence.intervals)){
  xx.lower <- cast(dc.data, threshold+cost.benefit.ratio~predictor, value = ifelse(standardize, "sNB_lower", "NB_lower"))
  xx.upper <- cast(dc.data, threshold+cost.benefit.ratio~predictor, value = ifelse(standardize, "sNB_upper", "NB_upper"))
 }else{
   xx.lower <- NULL
   xx.upper <- NULL
 }
  out <- list("plot.data" = list( "estimates" = xx.wide[,c("threshold", "cost.benefit.ratio", "all", predictors, "none")],
                                  "ci_lower" = xx.lower[,c("threshold", "cost.benefit.ratio", "all", predictors, "none")],
                                  "ci_upper" = xx.upper[,c("threshold", "cost.benefit.ratio", "all", predictors, "none")]),
              "derived.data"  = dc.data,
              "standardized" = standardize,
              "confidence.intervals" = confidence.intervals,
              "call" = call)
  class(out) = "DecisionCurve"
  invisible(out)

}



