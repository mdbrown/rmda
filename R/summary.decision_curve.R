#' Displays a useful description of a decision_curve object
#'
#' @param object decision_curve object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @param nround number of decimal places to round (default 3).
#' @method summary decision_curve
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

summary.decision_curve <- function(x, ..., measure = c("sNB", "NB", "TPR", "FPR"), nround = 3){

  #get measure name for printing
  measure <- match.arg(measure)
  measure.names.df <- data.frame(measure = c("sNB", "NB", "TPR", "FPR"), measure.names = c("Standardized Net Benefit",
                                                                                      "Net Benefit",
                                                                                      "Sensitivity",
                                                                                      "1-Specificity"))
  measure.name <- as.character(measure.names.df[match(measure, measure.names.df$measure), "measure.names"])

  #if this is true, confidence intervals have been calculated
  conf.int <- ncol(x$derived.data) > 9

  xx.wide <- cast(x$derived.data, thresholds+cost.benefit.ratio~method, value = measure)
  #need to add prob.high risk from the formula and convert to percent
  xx.wide$prob.high.risk <- subset(x$derived.data, !is.element(method, c("All", "None")))$prob.high.risk*100

  #rearrange terms to make sure we have the right ordering
  formula.name <- unique(x$derived.data$method)
  formula.name <- formula.name[!is.element(formula.name, c("None", "All"))]

  xx.wide <- xx.wide[, c("thresholds", "cost.benefit.ratio", "prob.high.risk", "All", formula.name, "None")]



  if( conf.int){

    xx.lower <- cast(x$derived.data, thresholds+cost.benefit.ratio~method, value = paste(measure, "_lower",sep = ""))
    xx.lower$prob.high.risk <- subset(x$derived.data, !is.element(method, c("All", "None")))$prob.high.risk_lower*100
    xx.lower <- xx.lower[, c("thresholds", "cost.benefit.ratio", "prob.high.risk", "All", formula.name, "None")]

    xx.upper <- cast(x$derived.data, thresholds+cost.benefit.ratio~method, value = paste(measure, "_upper",sep = ""))
    xx.upper$prob.high.risk <- subset(x$derived.data, !is.element(method, c("All", "None")))$prob.high.risk_upper*100
    xx.upper <- xx.upper[, c("thresholds", "cost.benefit.ratio", "prob.high.risk", "All", formula.name, "None")]

  }else{
    xx.lower <- NULL
    xx.upper <- NULL
  }

  out <- xx.wide
  out[,-c(1:2)] <- round(out[,-c(1:2)], nround)
  names(out)[1] <- "risk\nthreshold"
  names(out)[2] <- c("cost:benefit\n ratio")
  names(out)[3] <- c("percent\n high risk")



  if(conf.int){
    cat(paste0("\n", measure.name, " (", round(100*x$confidence.intervals),  "% Confidence Intervals):"))

    not.preds <- match(c("risk\nthreshold", "cost:benefit\n ratio",  "None"),
                       names(out))
    n.preds <- ncol(out) - length(not.preds)
    for( i in 1:n.preds){
       out[,-c(not.preds)][i] <- paste0(out[,-c(not.preds)][[i]],
                                 "\n(",
                                 round(c(unlist(xx.lower[,-c(not.preds)][[i]])), nround),
                                 ", ",
                                 round(c(unlist(xx.upper[,-c(not.preds)][[i]])), nround),
                                 ")")
    }
  }else{
    cat(paste0("\n", measure.name, ":"))

  }

  out.table <- pandoc.table(out, split.table = Inf, keep.line.breaks = TRUE)
  out.table
  cat('\n')

  invisible(out.table)
}
