check_DC_Inputs <- function(data,
                            outcome,
                            predictors,
                            thresholds = seq(0, 1, by = .01),
                            standardize = TRUE,
                            confidence.intervals, bootstraps,
                            legend.position = "topright",
                            cost.benefit.axis = TRUE,
                            cost.benefits,
                            n.cost.benefits = 6 ){

  #function to check all inputs, return vector of complete cases.

  #check data
  if(!is.data.frame(data)) stop("'data' must be a data.frame.")

  #check outcome
  if(!is.character(outcome)) stop("'outcome' must be a character string naming the outcome variable to be found in 'data'.")
  if(length(outcome) > 1) {
    warning("Only one outcome is allowed at a time. Only the first element will be used")
    outcome <- outcome[1]
  }
  if( !is.element(outcome, names(data))) stop("outcome is not found in data: outcome must be a character string of the name for the outcome variable found in 'data'.")
  if(!is.numeric(data[[outcome]])) stop("The outcome variable must refer to a numeric variable of 0's and 1's in 'data'.")
  if(!all(is.element(data[[outcome]], c(0,1)))) stop("The outcome variable must refer to a numeric variable of 0's and 1's in 'data'.")

  #check predictors
  if(!is.character(predictors)) stop("'predictors' must be a vector of character strings naming the predictor variables to be found in 'data'.")
  if( !all(is.element(predictors, names(data)))) stop("One or more predictors are not in names(data): predictors must be a vector of character strings giving the names for the predictor variables found in 'data'.")
  if( any(is.element(predictors, c("all", "none")))) stop("Predictors cannot be named 'all' or 'none'")

  for(p in predictors){
    if(min(data[[p]], na.rm = TRUE) < 0 | max(data[[p]], na.rm = TRUE) > 1){
      stop(paste("predictor", p, "shows risk predictions that are not inbetween 0 and 1. Decision curves cannot be calculated for such predictors."))
    }

  }

  #check for complete cases, and remove missing cases if necessary.
  data <- data[, c(outcome, predictors)]
  cc <- complete.cases(data)
  if(any(!(cc))){
    warning(paste(sum(!cc), "observations have missing data, and will be removed from analysis."))
  }

  #check thresholds
  stopifnot(is.numeric(thresholds))
  stopifnot(min(thresholds) >= 0 )
  stopifnot(max(thresholds) <= 1 )

  #check standardize
  stopifnot(is.logical(standardize))

  #check conf.int
  if(is.numeric(confidence.intervals) ){
    if(confidence.intervals <= 0 | confidence.intervals > 1) stop("confidence.intervals must be between 0 and 1 (or 'none'/NA if no confidence intervals are wanted).")
  }
  #check bootstraps
  stopifnot(is.numeric(bootstraps))
  stopifnot(bootstraps > 0 )

  #check legend.position
  if(!is.element(legend.position[1], c("none", "top",
                                       "topright", "right",
                                       "bottomright", "bottom",
                                       "bottomleft", "left", "topleft"))){
    stop('legend.position must be one of "none", "top","topright", "right", "bottomright", "bottom", "bottomleft", "left", "topleft"')
  }

  #check cost.benefit.axis
  stopifnot(is.logical(cost.benefit.axis))

  #check fits
  if(!missing(cost.benefits)){
    if( is.character(all.equal(grep(":", cost.benefits), 1:length(cost.benefits)))){
      stop("There was an error in how cost.benefits were input. 'cost.benefits', when specified, must be a character vector with elements 'xx:yy' with xx, yy representing the relative costs and benefits-for example c('1:2', '2:3', '1:1', '4:1').")
    }
  }

  #check nfits
  if(missing(cost.benefits)){
  stopifnot(is.numeric(n.cost.benefits))
  }


  return(cc)
}

