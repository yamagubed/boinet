
#' Fractional polynomial logistic regression
#'
#' Fractional polynomial (FP) logistic regression with two degrees of freedom is
#' performed to estimate the efficacy probabilities. The Best fitting FP model is
#' chosen by not taking into account the closed testing procedure.
#' @param obs Number of patients with events
#' @param n Number of patients
#' @param dose Dose
#' @return Estimated probability
#' @examples
#' fp.logit(obs=c(1,5,2),n=c(3,6,9),dose=c(1,2,3));
#' @import mfp
#' @export

fp.logit <- function(obs,n,dose)
{
  prob    <- obs/n
  ld      <- length(prob)
  obse.df <- NULL
  for(i in 1:ld){
    obse.df <- rbind(obse.df,data.frame(nEff=c(rep(1,obs[i]),rep(0,n[i]-obs[i])),dose=dose[i]))
  }
  fpfit <- suppressWarnings(mfp::mfp(factor(nEff)~fp(dose,df=4,select=0.99999,alpha=0.99999),family=binomial,data=obse.df))
  fpnum <- apply(as.matrix(dose),1,function(x){min((1:sum(n))[obse.df$dose==x])})
  fp.obspe <- as.numeric((fpfit$fit$fitted.values)[fpnum])
  return(fp.obspe)
}

