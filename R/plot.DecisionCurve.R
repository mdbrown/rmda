#'Plot a DecisionCurve object or many DecisionCurve objects
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

PlotDecisionCurve <- function(x, curve.names,
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
                               plot.type = c("net benefit", "sensitivity", "specificity", "impact"),
                               legend.position = c("topright", "right", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "none"),
                               ...){

  plot.type = match.arg(plot.type)
  legend.position = match.arg(legend.position)

  #extract data from x,
  #there is only one DecisionCurve to plot
  if(class(x) == "DecisionCurve"){

    dc.data <- x$derived.data

    if(missing(confidence.intervals)){
      confidence.intervals <- x$confidence.intervals
    }else{
      confidence.intervals <- ifelse(confidence.intervals, 1, "none")
    }
    if(missing(curve.names)){
    predictors <- unique(dc.data$method)
    predictors <- predictors[!is.element(predictors, c("none", "all"))]
    }else{
      dc.data$method[!is.element(dc.data$method, c("none", "all"))] <- curve.names[1]
      predictors <- curve.names[1]
    }

  }else if(class(x)=="list"){

    if(missing(confidence.intervals)){
      confidence.intervals <- x[[1]]$confidence.intervals
    }else{
      confidence.intervals <- ifelse(confidence.intervals, 1, "none")
    }

    message("Note: When multiple decision curves are plotted, decision curves for 'all' are calculated using the first DecisionCurve in the list provided.")

    #multiple dc's to plot
    #pull the "all' and 'none' curves from the first element in x
    dc.data <- subset(x[[1]]$derived.data, is.element(method, c("all", "none")))

    #fill in ci variables for if confidence intervals weren't calculated using DecisionCurve
    if(ncol(dc.data) == 9 ) dc.data <- add.ci.columns(dc.data)

    predictors <- NULL

    #loop through the remaining curves
    i = 0
    for(ll in x){
      i = i + 1
      #extract data to add
      newdata <-  subset(ll$derived.data, !is.element(method, c("all", "none")))
      #predictor name

      if(missing(curve.names)){
        #check to make sure the name is different
        newpred <- unique(newdata$method)
        if(is.element(newpred, predictors)) stop("After extracting the curve names from the DecisionCurve object, the names of the decision curves provided are the same for two or more DecisionCurve objects. Please set curve.names to avoid errors in plotting.")
      }else{
        newdata$method <- curve.names[i]
        newpred <- unique(newdata$method)
      }

      predictors <- c(predictors, newpred)

      #if confidence intervals weren't calculated
      if(ncol(newdata) == 9 ){
        if(confidence.intervals) warning(paste("confidence interval plotting were requested for curve '", newpred, "' but not calculated using DecisionCurve", sep = ''))
        #fill in ci variables for if confidence intervals weren't calculated using DecisionCurve
        newdata <- add.ci.columns(newdata)
      }


      dc.data <- rbind(dc.data, newdata)
    }


  }

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


  #plot the curves

    if(plot.type == "net benefit"){
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

    }else if(plot.type == "sensitivity"){
      plot_Sensitivity(xx = dc.data,
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
                       legend.position =  legend.position, ...)

    }else if(plot.type == "specificity"){
      plot_Specificity(xx = dc.data,
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
    }else if(plot.type == "impact"){
      plot_Impact(xx = dc.data,
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
                       legend.position =  legend.position, ...)
    }

}
