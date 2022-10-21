
#' Utility defined by truncated linear function
#'
#' Given estimated toxicity and efficacy probabilities, the utility which is
#' defined by truncated linear functions is Calculated.
#' @param probt Estimated toxicity probability
#' @param probe Estimated efficacy probability
#' @param tlow Lower bound for toxicity (default: 0.033)
#' @param tupp Upper bound for toxicity (default: 0.462)
#' @param elow Lower bound for efficacy (default: 0.21)
#' @param eupp Upper bound for efficacy (default: 0.70)
#' @return Utility value
#' @examples
#' utility.truncated.linear(probt=c(0.1,0.2,0.4),probe=c(0.2,0.6,0.6));
#' @export

utility.truncated.linear <- function(probt,probe,
                                     tlow=0.033,tupp=0.462,elow=0.21,eupp=0.70)
{
  ld  <- length(probt)
  fpe <- numeric(ld)
  fpt <- numeric(ld)

  for(d in 1:ld){
    if(probe[d]<=elow){
      fpe[d] <- 0
    }else if(probe[d]>=eupp){
      fpe[d] <- 1
    }else{
      fpe[d] <- (probe[d]-elow)/(eupp-elow)
    }
    if(probt[d]<=tlow){
      fpt[d] <- 1
    }else if(probt[d]>=tupp){
      fpt[d] <- 0
    }else{
      fpt[d] <- 1-(probt[d]-tlow)/(tupp-tlow)
    }
  }

  return(fpe*fpt)
}

