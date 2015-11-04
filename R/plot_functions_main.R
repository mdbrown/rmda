#'Plot a decision.curve object or many decision.curve objects
#'
#' @param x DecisionCurve object to plot or a list of DecisionCurve objects. Assumes output from function 'DecisionCurve'
#' @param ... other arguments ignored (for compatibility with generic)
#' @param nround number of decimal places to round (default 3).
#' @param cost.benefit.axis logical (default TRUE) indicating whether to print an additional x-axis showing relative cost:benefit ratios in addition to risk thresholds.
#' @param n.cost.benefits number of cost:benefit ratios to print if cost.benefit.axis = TRUE (default n.cost.benefit = 6).
#' @param cost.benefits Character vector of the form c("c1:b1", "c2:b2", ..., "cn:bn") with integers ci, bi corresponding to specific cost:benefit ratios to print.
#' @param standardize logical (default TRUE) indicating whether to use the standardized net benefit (NB/disease prevalence) or not.
#' @param col vector of color names to be used in plotting corresponding to the 'predictors' given. Default colors will be chosen from rainbow(..., v = .8). See details for more information on plot parameters.
#' @param lty vector of linetypes to plot corresponding to 'predictors'.
#' @param lwd vector of linewidths to plot corresponding to 'predictors'.
#' @param xlim vector giving c(min, max) of x-axis. Defaults to c(min(thresholds), max(thresholds)).
#' @param ylim vector giving c(min, max) of y-axis.
#' @param xlab label of main x-axis.
#' @param ylab label of y-axis.
#' @param cost.benefit.xlab label of cost:benefit ratio axis.
#' @param ... other options directly send to plot()
#'
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
#'my.dc <- DecisionCurve(dcaData,
#'                       outcome = "Cancer", predictors = c("BasicModel", "FullModel"),
#'                       bootstraps = 25,
#'                       thresholds = seq(0, 0.4, by = .05))
#'
#'summary(my.dc)
#'
#'
#' @export

plot_decision_curve <- function(x, curve.names,
                               cost.benefit.axis = TRUE,
                               n.cost.benefits = 6,
                               cost.benefits,
                               standardize = TRUE,
                               confidence.intervals,
                               col,
                               lty, lwd = 2,
                               xlim, ylim,
                               xlab = "Risk Threshold", ylab,
                               cost.benefit.xlab = "Cost:Benefit Ratio",
                               legend.position = c("topright", "right", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "none"),
                               ...){

  legend.position <- match.arg(legend.position)

  if(missing(curve.names)) curve.names  <- NA
  if(missing(confidence.intervals)) confidence.intervals <- NA

  prepData <- preparePlotData(x = x,
                             curve.names = curve.names,
                             confidence.intervals = confidence.intervals)

  predictors <- prepData$predictors
  dc.data <- prepData$dc.data
  confidence.intervals <- prepData$confidence.intervals
  rm(prepData)

  #set some defaults if needed
  if(missing(xlim)) xlim = range(dc.data$thresholds)

  if(missing(lty)) lty = rep(1, length(predictors) + 2)
  if(length(lty) ==1) lty = rep(lty, length(predictors) + 2)
  if(length(lty) == length(predictors)) lty = c(lty, 1, 1)

  if(missing(col)) col  = c(rainbow(length(predictors), v = .8), "grey66", "black")
  if(length(col) == length(predictors)) col <- c(col, "grey66", "black")

  if(missing(lwd)) lwd = 2
  if(length(lwd) ==1) lwd <- rep(lwd, length(predictors))
  if(length(lwd) == length(predictors)) lwd = c(lwd, 1, 1)

  if(missing(ylab)) ylab <- ifelse(standardize, "Standardized Net Benefit", "Net Benefit")

    if(missing(ylim)){

    if(standardize) ylim = c(-1, 1)
    else ylim = c(-0.05, max(xx[[value]][is.finite(xx[[value]])]))

  }

  plot_generic(xx = dc.data,
               predictors = predictors,
               value = ifelse(standardize, "sNB", "NB"),
               plotNew = TRUE,
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

#'Plot the sensitivity of a DecisionCurve object or many DecisionCurve objects
#'
#' @param x DecisionCurve object to plot or a list of DecisionCurve objects. Assumes output from function 'DecisionCurve'
#' @param ... other arguments ignored (for compatibility with generic)
#' @param nround number of decimal places to round (default 3).
#' @param cost.benefit.axis logical (default TRUE) indicating whether to print an additional x-axis showing relative cost:benefit ratios in addition to risk thresholds.
#' @param n.cost.benefits number of cost:benefit ratios to print if cost.benefit.axis = TRUE (default n.cost.benefit = 6).
#' @param cost.benefits Character vector of the form c("c1:b1", "c2:b2", ..., "cn:bn") with integers ci, bi corresponding to specific cost:benefit ratios to print.
#' @param standardize logical (default TRUE) indicating whether to use the standardized net benefit (NB/disease prevalence) or not.
#' @param col vector of color names to be used in plotting corresponding to the 'predictors' given. Default colors will be chosen from rainbow(..., v = .8). See details for more information on plot parameters.
#' @param lty vector of linetypes to plot corresponding to 'predictors'.
#' @param lwd vector of linewidths to plot corresponding to 'predictors'.
#' @param xlim vector giving c(min, max) of x-axis. Defaults to c(min(thresholds), max(thresholds)).
#' @param ylim vector giving c(min, max) of y-axis.
#' @param xlab label of main x-axis.
#' @param ylab label of y-axis.
#' @param cost.benefit.xlab label of cost:benefit ratio axis.
#' @param ... other options directly send to plot()
#'
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
#'my.dc <- DecisionCurve(dcaData,
#'                       outcome = "Cancer", predictors = c("BasicModel", "FullModel"),
#'                       bootstraps = 25,
#'                       thresholds = seq(0, 0.4, by = .05))
#'
#'summary(my.dc)
#'
#'
#' @export


plot_roc_components <- function(x,
                              cost.benefit.axis = TRUE,
                              n.cost.benefits = 6,
                              cost.benefits,
                              confidence.intervals,
                              col = "black",
                              lty.fpr = 2,
                              lty.tpr = 1,
                              lwd = 2,
                              xlim, ylim,
                              xlab = "Risk Threshold", ylab,
                              cost.benefit.xlab = "Cost:Benefit Ratio",
                              legend.position = c("topright", "right", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "none"),
                              ...){

  if(class(x) != "decision_curve") stop("x must be an object of class 'decision_curve'-- plot_roc_components is only functional for one decision curve at a time.")

  legend.position <- match.arg(legend.position)

 # if(missing(curve.names)) curve.names  <- NA
  if(missing(confidence.intervals)) confidence.intervals <- NA

  prepData <- preparePlotData(x = x,
                              curve.names = NA,
                              confidence.intervals = confidence.intervals)

  predictors <- prepData$predictors
  dc.data <- prepData$dc.data
  confidence.intervals <- prepData$confidence.intervals
  rm(prepData)

  #set some defaults if needed
  if(missing(xlim)) xlim = range(dc.data$thresholds)

  if(length(col) ==1 ) col <- rep(col, 2)

  if(missing(lwd)) lwd = 2
  if(length(lwd) ==1) lwd <- rep(lwd, length(predictors))
  if(length(lwd) == length(predictors)) lwd = c(lwd, 1, 1)

  if(missing(ylab)) ylab <- "Probability"

  if(missing(ylim)) ylim = c(0, 1)


  plot_generic(xx = dc.data,
               predictors = predictors,
               value = "TPR",
               plotNew = TRUE,
               standardize = FALSE,
               confidence.intervals,
               cost.benefit.axis = cost.benefit.axis,
               cost.benefits = cost.benefits,
               n.cost.benefits = n.cost.benefits,
               cost.benefit.xlab = cost.benefit.xlab,
               xlab = xlab, ylab = ylab,
               col = col,
               lty = lty.tpr, lwd = lwd,
               xlim = xlim, ylim = ylim,
               legend.position = "none",
               ...)

  plot_generic(xx = dc.data,
               predictors = predictors,
               value = "FPR",
               plotNew = FALSE,
               standardize = FALSE,
               confidence.intervals,
               cost.benefit.axis = cost.benefit.axis,
               cost.benefits = cost.benefits,
               n.cost.benefits = n.cost.benefits,
               cost.benefit.xlab = cost.benefit.xlab,
               xlab = xlab, ylab = ylab,
               col = col,
               lty = lty.fpr, lwd = lwd,
               xlim = xlim, ylim = ylim,
               legend.position = legend.position,
               lty.fpr = lty.fpr,
               lty.tpr = lty.tpr,
               tpr.fpr.legend = TRUE,
               ...)



}

#'Plot the clinical impact curve from a DecisionCurve object or many DecisionCurve objects
#'
#' @param x DecisionCurve object to plot or a list of DecisionCurve objects. Assumes output from function 'DecisionCurve'
#' @param ... other arguments ignored (for compatibility with generic)
#' @param nround number of decimal places to round (default 3).
#' @param cost.benefit.axis logical (default TRUE) indicating whether to print an additional x-axis showing relative cost:benefit ratios in addition to risk thresholds.
#' @param n.cost.benefits number of cost:benefit ratios to print if cost.benefit.axis = TRUE (default n.cost.benefit = 6).
#' @param cost.benefits Character vector of the form c("c1:b1", "c2:b2", ..., "cn:bn") with integers ci, bi corresponding to specific cost:benefit ratios to print.
#' @param standardize logical (default TRUE) indicating whether to use the standardized net benefit (NB/disease prevalence) or not.
#' @param col vector of color names to be used in plotting corresponding to the 'predictors' given. Default colors will be chosen from rainbow(..., v = .8). See details for more information on plot parameters.
#' @param lty vector of linetypes to plot corresponding to 'predictors'.
#' @param lwd vector of linewidths to plot corresponding to 'predictors'.
#' @param xlim vector giving c(min, max) of x-axis. Defaults to c(min(thresholds), max(thresholds)).
#' @param ylim vector giving c(min, max) of y-axis.
#' @param xlab label of main x-axis.
#' @param ylab label of y-axis.
#' @param cost.benefit.xlab label of cost:benefit ratio axis.
#' @param ... other options directly send to plot()
#'
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
#'my.dc <- DecisionCurve(dcaData,
#'                       outcome = "Cancer", predictors = c("BasicModel", "FullModel"),
#'                       bootstraps = 25,
#'                       thresholds = seq(0, 0.4, by = .05))
#'
#'summary(my.dc)
#'
#'
#' @export

plot_clinical_impact <- function(x,
                                 population.size = 1000,
                            cost.benefit.axis = TRUE,
                            n.cost.benefits = 6,
                            cost.benefits,
                            standardize = TRUE,
                            confidence.intervals,
                            col = "black",
                            lwd = 2,
                            xlim, ylim,
                            xlab = "Risk Threshold", ylab,
                            cost.benefit.xlab = "Cost:Benefit Ratio",
                            legend.position = c("topright", "right", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "none"),
                            ...){

  if(class(x) != "decision_curve") stop("x must be an object of class 'decision_curve'-- plot_roc_components is only functional for one decision curve at a time.")

  legend.position <- match.arg(legend.position)

  #if(missing(curve.names)) curve.names  <- NA
  if(missing(confidence.intervals)) confidence.intervals <- NA

  prepData <- preparePlotData(x = x,
                              curve.names = NA,
                              confidence.intervals = confidence.intervals)

  predictors <- prepData$predictors
  dc.data <- prepData$dc.data
  confidence.intervals <- prepData$confidence.intervals
  rm(prepData)

  #set some defaults if needed
  if(missing(xlim)) xlim = range(dc.data$thresholds)

  if(length(col) ==1 ) col <- rep(col, 2)

  if(missing(lwd)) lwd = 2
  if(length(lwd) ==1) lwd <- rep(lwd, length(predictors))
  if(length(lwd) == length(predictors)) lwd = c(lwd, 1, 1)

  if(missing(ylab)) ylab <-  paste("Number high risk (out of ", population.size,  ")", sep = "")

  if(missing(ylim)) ylim = c(0, population.size*1.05)

  plot_generic(xx = dc.data,
               predictors = predictors,
               value = "prob.high.risk",
               plotNew = TRUE,
               standardize = FALSE,
               confidence.intervals,
               cost.benefit.axis = cost.benefit.axis,
               cost.benefits = cost.benefits,
               n.cost.benefits = n.cost.benefits,
               cost.benefit.xlab = cost.benefit.xlab,
               xlab = xlab, ylab = ylab,
               col = col,
               lty = 1, lwd = lwd,
               xlim = xlim, ylim = ylim,
               legend.position = "none",
               population.size = population.size,
               ...) #add my own legend

  plot_generic(xx = dc.data,
               predictors = predictors,
               value = "DP",
               plotNew = FALSE,
               standardize = FALSE,
               confidence.intervals,
               cost.benefit.axis = cost.benefit.axis,
               cost.benefits = cost.benefits,
               n.cost.benefits = n.cost.benefits,
               cost.benefit.xlab = cost.benefit.xlab,
               xlab = xlab, ylab = ylab,
               col = col,
               lty = 2, lwd = lwd,
               xlim = xlim, ylim = ylim,
               legend.position = legend.position,
               lty.fpr = 0,
               lty.tpr = 0,
               tpr.fpr.legend = FALSE,
               impact.legend = TRUE,
               population.size = population.size,
               ...)

}
