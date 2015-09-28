plot_DecisionCurve <- function(xx,predictors,  standardize, confidence.intervals,
                               cost.benefit.axis = TRUE, cost.benefits, n.cost.benefits,
                               cost.benefit.xlab, xlab, ylab,
                               col, lty, lwd, xlim, ylim, legend.position, ...){
## xx is output from get_DecisionCurve,
## others are directly from the function call

  #save old par parameters and reset them once the function exits.
  old.par<- par("mar"); on.exit(par(mar = old.par))

  xx.wide <- cast(xx, thresholds~method, value = ifelse(standardize, "sNB", "NB"))
  if(is.numeric(confidence.intervals)){
    xx.lower <- cast(xx, thresholds~method, value = ifelse(standardize, "sNB_lower", "NB_lower"))
    xx.upper <- cast(xx, thresholds~method, value = ifelse(standardize, "sNB_upper", "NB_upper"))

  }

  # adjust margins to add extra x-axis
  if(cost.benefit.axis) par(mar = c(7.5, 4, 3, 2) + 0.1)

  #set default ylim if not provided
  if(missing(ylim)){

    if(standardize) ylim = c(-1, 1)
    else ylim = c(-0.05, max(xx$NB[is.finite(xx$NB)]))

  }
  if(missing(ylab)) ylab = ifelse(standardize, "Standardized Net Benefit", "Net Benefit")

  #initial call to plot and add gridlines
  plot(xx.wide$thresholds, xx.wide$none, type = "n", ylim = ylim,
       col = "black", xlim = xlim,  xlab = "", ylab = ylab, frame.plot = FALSE, ...)

  grid(lty = 1, col = "grey92")

  #plot none and all
  lines(xx.wide$thresholds, xx.wide$none, type = "l",
        col = col[length(predictors)+ 2],
        lty = lty[length(predictors)+ 2],
        lwd = lwd[length(predictors)+ 2])

  lines(xx.wide$threshold, xx.wide$all, type = "l",
        col = col[length(predictors)+ 1],
        lty = lty[length(predictors)+ 1],
        lwd = lwd[length(predictors)+ 1])

  if(is.numeric(confidence.intervals)){
    lines(xx.lower[,c("thresholds", "all")],
          col = col[length(predictors)+ 1],
          lty = lty[length(predictors)+ 1],
          lwd = lwd[length(predictors)+ 1]/2)

    lines(xx.upper[,c("thresholds", "all")],
          col = col[length(predictors)+ 1],
          lty = lty[length(predictors)+ 1],
          lwd = lwd[length(predictors)+ 1]/2)

  }

  #plot each predictor
  for(i in 1:length(predictors)){
    #plot ci's if asked for
    if(is.numeric(confidence.intervals)){
      lines(xx.lower[,c("thresholds", predictors[i])],
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)
      lines(xx.upper[,c("thresholds", predictors[i])],
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

    }

     lines(xx.wide[,c("thresholds", predictors[i])],
           type = "l",  col = col[i], lty = lty[i], lwd = lwd[i])


  }

  #add legend
  if(is.element(legend.position, c("bottomright", "topright", "bottomleft", "topleft", "right", "left", "top", "bottom"))){
    legend(legend.position, lty = lty, col = col, lwd = lwd, legend = c(predictors, "all", "none"))
  }

  #add cost benefit axis if wanted
  if(cost.benefit.axis){
    tmp <- Add_CostBenefit_Axis(xlim = xlim,
                                cost.benefits = cost.benefits,
                                n.cost.benefits = n.cost.benefits,
                                line = 4)
    mtext(xlab, 1, 2.2)
    mtext(cost.benefit.xlab, side = 1, 6.1)
  }else{
    mtext(xlab, side = 1, 3)
  }
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################

plot_Sensitivity <- function(xx,predictors,  standardize, confidence.intervals,
                               cost.benefit.axis = TRUE, cost.benefits, n.cost.benefits,
                               cost.benefit.xlab, xlab, ylab,
                               col, lty, lwd, xlim, ylim, legend.position, ...){
  ## xx is output from get_DecisionCurve,
  ## others are directly from the function call

  #save old par parameters and reset them once the function exits.
  old.par<- par("mar"); on.exit(par(mar = old.par))


  xx.wide <- cast(xx, thresholds~method, value = "TPF")
  if(is.numeric(confidence.intervals)){
    xx.lower <- cast(xx, thresholds~method, value =  "TPF_lower")
    xx.upper <- cast(xx, thresholds~method, value = "TPF_upper")

  }

  # adjust margins to add extra x-axis
  if(cost.benefit.axis) par(mar = c(7.5, 4, 3, 2) + 0.1)

  #set default ylim if not provided
  if(missing(ylim)) ylim = c(0, 1)
  if(missing(ylab)) ylab =  "Sensitivity"

  #initial call to plot and add gridlines
  plot(xx.wide$thresholds, xx.wide$TPF, type = "n", ylim = ylim,
       col = "black", xlim = xlim,  xlab = "", ylab = ylab, frame.plot = FALSE, ...)

  grid(lty = 1, col = "grey92")

  #plot each predictor
  for(i in 1:length(predictors)){
    #plot ci's if asked for
    if(is.numeric(confidence.intervals)){
      lines(xx.lower[,c("thresholds", predictors[i])],
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)
      lines(xx.upper[,c("thresholds", predictors[i])],
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

    }

    lines(xx.wide[,c("thresholds", predictors[i])],
          type = "l",  col = col[i], lty = lty[i], lwd = lwd[i])


  }

  #add legend
  if(is.element(legend.position, c("bottomright", "topright", "bottomleft", "topleft", "right", "left", "top", "bottom"))){
   n.pred = length(predictors)
   legend(legend.position, lty = lty[1:n.pred], col = col[1:n.pred], lwd = lwd[1:n.pred], legend = c(predictors))
  }

  #add cost benefit axis if wanted
  if(cost.benefit.axis){
    tmp <- Add_CostBenefit_Axis(xlim = xlim,
                                cost.benefits = cost.benefits,
                                n.cost.benefits = n.cost.benefits,
                                line = 4)
    mtext(xlab, 1, 2.2)
    mtext(cost.benefit.xlab, side = 1, 6.1)
  }else{
    mtext(xlab, side = 1, 3)
  }
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################


plot_Specificity <- function(xx,predictors,  standardize, confidence.intervals,
                             cost.benefit.axis = TRUE, cost.benefits, n.cost.benefits,
                             cost.benefit.xlab, xlab, ylab,
                             col, lty, lwd, xlim, ylim, legend.position, ...){
  ## xx is output from get_DecisionCurve,
  ## others are directly from the function call

  #save old par parameters and reset them once the function exits.
  old.par<- par("mar"); on.exit(par(mar = old.par))


  xx.wide <- cast(xx, thresholds~method, value = "FPF")
  if(is.numeric(confidence.intervals)){
    xx.lower <- cast(xx, thresholds~method, value =  "FPF_lower")
    xx.upper <- cast(xx, thresholds~method, value = "FPF_upper")

  }

  # adjust margins to add extra x-axis
  if(cost.benefit.axis) par(mar = c(7.5, 4, 3, 2) + 0.1)

  #set default ylim if not provided
  if(missing(ylim)) ylim = c(0, 1)
  if(missing(ylab)) ylab =  "1-Specificity"

  #initial call to plot and add gridlines
  plot(xx.wide$thresholds, xx.wide$FPF, type = "n", ylim = ylim,
       col = "black", xlim = xlim,  xlab = "", ylab = ylab, frame.plot = FALSE, ...)

  grid(lty = 1, col = "grey92")

  #plot each predictor
  for(i in 1:length(predictors)){
    #plot ci's if asked for
    if(is.numeric(confidence.intervals)){
      lines(xx.lower[,c("thresholds", predictors[i])],
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)
      lines(xx.upper[,c("thresholds", predictors[i])],
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

    }

    lines(xx.wide[,c("thresholds", predictors[i])],
          type = "l",  col = col[i], lty = lty[i], lwd = lwd[i])


  }

  #add legend
  if(is.element(legend.position, c("bottomright", "topright", "bottomleft", "topleft", "right", "left", "top", "bottom"))){
    n.pred = length(predictors)
    legend(legend.position, lty = lty[1:n.pred], col = col[1:n.pred], lwd = lwd[1:n.pred], legend = c(predictors))
  }

  #add cost benefit axis if wanted
  if(cost.benefit.axis){
    tmp <- Add_CostBenefit_Axis(xlim = xlim,
                                cost.benefits = cost.benefits,
                                n.cost.benefits = n.cost.benefits,
                                line = 4)
    mtext(xlab, 1, 2.2)
    mtext(cost.benefit.xlab, side = 1, 6.1)
  }else{
    mtext(xlab, side = 1, 3)
  }
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################

plot_Impact<- function(xx,predictors,  standardize, confidence.intervals,
                             cost.benefit.axis = TRUE, cost.benefits, n.cost.benefits,
                             cost.benefit.xlab, xlab, ylab,
                             col, lty, lwd, xlim, ylim, legend.position, ...){
  ## xx is output from get_DecisionCurve,
  ## others are directly from the function call

  #save old par parameters and reset them once the function exits.
  old.par<- par("mar"); on.exit(par(mar = old.par))

  xx.wide <- cast(xx, thresholds~method, value = "prob.high.risk")

  if(is.numeric(confidence.intervals)){
    xx.lower <- cast(xx, thresholds~method, value =  "prob.high.risk_lower")
    xx.upper <- cast(xx, thresholds~method, value =  "prob.high.risk_upper")
  }

  # adjust margins to add extra x-axis
  if(cost.benefit.axis) par(mar = c(7.5, 4, 3, 2) + 0.1)

  #set default ylim if not provided
  if(missing(ylim))  ylim = c(1000, 0)
  if(missing(ylab)) ylab =  "Number high risk (out of 1000)"

  #initial call to plot and add gridlines
  plot(xx.wide$thresholds, xx.wide$none, type = "n", ylim = ylim,
       col = "black", xlim = xlim,  xlab = "", ylab = ylab, frame.plot = FALSE, ...)

  grid(lty = 1, col = "grey92")

  for(i in 1:length(predictors)){
    #plot ci's if asked for
    if(is.numeric(confidence.intervals)){

      lines(xx.lower[,c("thresholds")], y = (1-xx.lower[,c(predictors[i])])*1000,
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

      lines(xx.upper[,c("thresholds")], y = (1-xx.upper[,c(predictors[i])])*1000,
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

    }

    lines(xx.wide[,c("thresholds")], y = (1-xx.wide[,c(predictors[i])])*1000,
          type = "l",  col = col[i], lty = lty[i], lwd = lwd[i])


  }


  xx.wide <- cast(xx, thresholds~method, value = "DP")

  if(is.numeric(confidence.intervals)){

    xx.lower <- cast(xx, thresholds~method, value =  "DP_lower")
    xx.upper <- cast(xx, thresholds~method, value =  "DP_upper")
  }


  for(i in 1:length(predictors)){
    #plot ci's if asked for
    if(is.numeric(confidence.intervals)){

      lines(xx.lower[,c("thresholds")], y = (1-xx.lower[,c(predictors[i])])*1000,
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

      lines(xx.upper[,c("thresholds")], y = (1-xx.upper[,c(predictors[i])])*1000,
            type = "l",  col = col[i], lty = lty[i], lwd = lwd[i]/2)

    }

    lines(xx.wide[,c("thresholds")], y = (1-xx.wide[,c(predictors[i])])*1000,
          type = "l",  col = col[i], lty = lty[i], lwd = lwd[i])


  }
  #add legend
  if(is.element(legend.position, c("bottomright", "topright", "bottomleft", "topleft", "right", "left", "top", "bottom"))){
    n.pred = length(predictors)
    legend(legend.position, lty = lty[1:n.pred], col = col[1:n.pred], lwd = lwd[1:n.pred], legend = c(predictors))
  }

  #add cost benefit axis if wanted
  if(cost.benefit.axis){
    tmp <- Add_CostBenefit_Axis(xlim = xlim,
                                cost.benefits = cost.benefits,
                                n.cost.benefits = n.cost.benefits,
                                line = 4)
    mtext(xlab, 1, 2.2)
    mtext(cost.benefit.xlab, side = 1, 6.1)
  }else{
    mtext(xlab, side = 1, 3)
  }
}

