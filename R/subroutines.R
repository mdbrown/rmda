


calculate.nb <- function(y, d, rH){
  #predictor y,  estimated risk
  #disease indicator d
  #vector of high risk thresholds rH

  N = length(y)

  #denominator for tpf
  tpf.den <- sum(d ==1)

  #true positive fraction Pr(risk(X) > rH | D = 1)
  tpf <- sum.I(rH, "<", y[d==1])/tpf.den

  #false positive fraction Pr(risk(X) > rH | D = 0 )
  fpf <- sum.I(rH, "<", y[d==0])/ (N - tpf.den)

  #disease prevalence
  rho = mean(d==1)

  #net benefit
  nb = tpf*rho - (rH/(1-rH))*(1-rho)*fpf

  #standardized net benefit
  snb = nb/rho

  out  = data.frame("threshold" = rH,  "FPF" = fpf ,"TPF" = tpf, "NB" = nb, "sNB" = snb, "rho" = rho)

 # AUC   = sum(sort(tpf, decreasing = FALSE)*(sort(fpf, decreasing = FALSE)-c(sort(fpf, decreasing = FALSE)[-1],0)))


}






VTM <- function(vc, dm)
{
  matrix(vc, ncol = length(vc), nrow = dm, byrow = T)
}



sum.I<-function(yy,FUN,Yi,Vi=NULL){

  if (FUN=="<"|FUN==">=") { yy <- -yy; Yi <- -Yi}

  pos <- rank(c(yy,Yi),ties.method='f')[1:length(yy)]-rank(yy,ties.method='f')

  if (substring(FUN,2,2)=="=") pos <- length(Yi)-pos

  if (!is.null(Vi)) {

    if(substring(FUN,2,2)=="=") tmpind <- order(-Yi) else  tmpind <- order(Yi)

    Vi <- apply(as.matrix(Vi)[tmpind,,drop=F],2,cumsum)

    return(rbind(0,Vi)[pos+1,])

  } else return(pos)
}


costbenefit_to_threshold <- function(CB){
  CB/(1+CB)
}

threshold_to_costbenefit <- function(rh){
  rh/(1-rh)
}
