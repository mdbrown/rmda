#' Displays a useful description of a DecisionCurve object
#'
#' @param object DecisionCurve object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @param nround number of decimal places to round (default 3).
#' @method summary DecisionCurve
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

summary.DecisionCurve <- function(x, ..., nround = 3){

  out <- x$plot.data$estimates
  out[,-c(1:2)] <- round(out[,-c(1:2)], nround)
  names(out)[1] <- "risk\nthreshold"
  names(out)[2] <- c("cost:benefit\n ratio")

  if(!is.null(x$plot.data$ci_lower)){
    if(x$standardize){
      cat(paste0("\nStandardized Net Benefit (", round(100*x$confidence.intervals),  "% Confidence Intervals):"))
    }else{
      cat(paste0("\nNet Benefit (", round(100*x$confidence.intervals),  "% Confidence Intervals):"))
    }
    not.preds <- match(c("risk\nthreshold", "cost:benefit\n ratio", "all", "none"),
                       names(out))
    n.preds <- ncol(out) - length(not.preds)
    for( i in 1:n.preds){
       out[,-c(not.preds)][i] <- paste0(out[,-c(not.preds)][[i]],
                                 "\n(",
                                 round(c(unlist(x$plot.data$ci_lower[,-c(not.preds)][[i]])), nround),
                                 ", ",
                                 round(c(unlist(x$plot.data$ci_upper[,-c(not.preds)][[i]])), nround),
                                 ")")
    }
  }else{
    if(x$standardize){
      cat("\nStandardized Net Benefit:")
    }else{
      cat("\nNet Benefit:")
    }
  }

  pandoc.table(out, split.table = Inf, keep.line.breaks = TRUE)
  cat('\n')
}
