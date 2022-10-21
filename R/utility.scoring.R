
#' Utility defined by scoring
#'
#' Given estimated toxicity and efficacy probabilities, the utility which is
#' defined by scoring is calculated.
#' @param probt Estimated toxicity probability
#' @param probe Estimated efficacy probability
#' @param psi00 Score for toxicity=no and efficacy=no (default: 40)
#' @param psi11 Score for toxicity=yes and efficacy=yes (default: 60)
#' @return Utility value
#' @examples
#' utility.scoring(probt=c(0.1,0.2,0.4),probe=c(0.2,0.6,0.6));
#' @export

utility.scoring <- function(probt,probe,
                            psi00=40,psi11=60)
{
  psi.e0t1 <- 0
  psi.e0t0 <- psi00
  psi.e1t1 <- psi11
  psi.e1t0 <- 100

  ut = (  psi.e0t1*(1-probe)*probt
        + psi.e0t0*(1-probe)*(1-probt)
        + psi.e1t1*probe    *probt
        + psi.e1t0*probe    *(1-probt))

  return(ut)
}

