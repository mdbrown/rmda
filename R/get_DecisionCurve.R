get_DecisionCurve <- function(data, outcome, predictors, threshold, standardize, confidence.intervals, bootstraps){


  outcome <- data[[outcome]]
  predictors <- c(predictors, "all", "none")
  data[["all"]] <- 1
  data[["none"]] <- 0
  n.preds <- length(predictors)

  out <- data.frame("threshold" = numeric(length(predictors)*length(threshold)),
                    "FPF" = numeric(length(predictors)*length(threshold)),
                    "TPF" = numeric(length(predictors)*length(threshold)),
                    "NB" = numeric(length(predictors)*length(threshold)),
                    "sNB" = numeric(length(predictors)*length(threshold)),
                    "rho" = numeric(length(predictors)*length(threshold)),
                    "predictor"= numeric(length(predictors)*length(threshold)))

  #if ci's
  #calculate bootstrap indices
  if(is.numeric(confidence.intervals))  {
    rtn <- ifelse(standardize, "sNB", "NB")

    B.ind <- matrix(nrow = nrow(data), ncol = bootstraps)
    for(b in 1:bootstraps) B.ind[,b] <- sample.int(nrow(data), replace = TRUE)

    out[[paste(rtn, "_lower", sep = "")]] <- NA
    out[[paste(rtn, "_upper", sep = "")]] <- NA

  }

  index = 1
  n.pred = 1
  for(i in 1:n.preds){

    tmpNBdata <- calculate.nb(d = outcome,
                              y = data[[predictors[i]]],
                              rH = threshold)#data[[predictors[i]]])

    tmpNBdata$predictor <- predictors[[i]]

    if(is.numeric(confidence.intervals)){
      #calculate NB or sNB
      boot.data <- apply(B.ind, 2, function(x){
                                    calculate.nb(d = outcome[x], y = data[[predictors[i]]][x], rH = threshold)[[rtn]]})
      alpha = 1- confidence.intervals
      tmpNBdata[[paste(rtn, "_lower", sep = "")]] <- apply(boot.data, 1, quantile, probs = alpha/2, type = 1, na.rm = TRUE)
      tmpNBdata[[paste(rtn, "_upper", sep = "")]] <- apply(boot.data, 1, quantile, probs = 1-alpha/2, type = 1, na.rm = TRUE)

    }


    out[index:(length(threshold)*n.pred),] <-  tmpNBdata
    index = index + length(threshold)
    n.pred = n.pred + 1
  }


  out$cost.benefit.ratio <- as.character(fractions(threshold_to_costbenefit(out$threshold)))
  #find indices without a fraction and make them "xx/1"
  add.dash1 <-  which(!is.element(1:nrow(out), grep("/", out$cost.benefit.ratio)))
  out$cost.benefit.ratio[add.dash1] <- paste(out$cost.benefit.ratio[add.dash1], "/1", sep = "")
  out$cost.benefit.ratio <- gsub("/", ":", out$cost.benefit.ratio)


  out
}





