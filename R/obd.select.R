
#' Optimal biological dose selection
#'
#' Optimal biological dose (OBD) is selected based on a method specified.
#' @param probt Estimated toxicity probability (default: c(0.1,0.2,0.4))
#' @param probe Estimated efficacy probability (default: c(0.2,0.6,0.6))
#' @param method Method used for OBD selection
#' @param phi Target toxicity probability (default: 0.33)
#' @param phi1 Lower bound of toxicity probability (default: 0.033)
#' @param phi2 Upper bound of toxicity probability (default: 0.462)
#' @param delta Target efficacy probability (default: 0.70)
#' @param delta1 Lower bound of efficacy probability (default: 0.42)
#' @param tterm Probability of meeting toxicity stopping criteria (default: c(0.5,0.5,0.5))
#' @param eterm Probability of meeting efficacy stopping criteria (default: c(0.5,0.5,0.5))
#' @param stopT Toxicity stopping criteria (default: 0.95)
#' @param stopE Efficacy stopping criteria (default: 0.95)
#' @param w1 Weight for toxicity-efficacy trade-off (default: 0.33)
#' @param w2 Weight for penalty imposed on toxic doses (default: 1.09)
#' @param tox.upper Upper bound of toxicity probability (default: 0.462)
#' @param psi00 Score for toxicity=no and efficacy=no (default: 40)
#' @param psi11 Score for toxicity=yes and efficacy=yes (default: 60)
#' @return OBD
#' @examples
#' obd.select(method="max.effprob");
#' @export

obd.select <- function(
                probt     = c(0.1,0.2,0.4),
                probe     = c(0.2,0.6,0.6),
                method    = c("utility.weighted","utility.truncated.linear","utility.scoring","max.effprob"),
                phi       = 0.33,
                phi1      = 0.033,
                phi2      = 0.462,
                delta     = 0.70,
                delta1    = 0.42,
                tterm     = c(0.5,0.5,0.5),
                eterm     = c(0.5,0.5,0.5),
                stopT     = 0.95,
                stopE     = 0.95,
                w1        = 0.33,
                w2        = 1.09,
                tox.upper = 0.462,
                psi00     = 40,
                psi11     = 60)
{
  candose <- which((tterm>=(1-stopT))&(eterm>=(1-stopE)))

  if(method=="utility.weighted"){
    fu <- utility.weighted(probt=probt,probe=probe,
                           w1=w1,w2=w2,tox.upper=tox.upper)
    fu.max <- which(fu==max(fu[candose]))
    re     <- min(intersect(fu.max,candose))

  }else if(method=="utility.truncated.linear"){
    fu <- utility.truncated.linear(probt=probt,probe=probe,
                                   tlow=phi1,tupp=phi2,elow=(delta1/2),eupp=delta)
    fu.max <- which(fu==max(fu[candose]))
    re     <- min(intersect(fu.max,candose))

  }else if(method=="utility.scoring"){
    fu <- utility.scoring(probt=probt,probe=probe,
                          psi00=psi00,psi11=psi11)
    fu.max <- which(fu==max(fu[candose]))
    re     <- min(intersect(fu.max,candose))

  }else if(method=="max.effprob"){
    mdif <- min(abs(probt[candose]-phi))
    mtd  <- max(which(abs(probt-phi)==mdif))
    meff <- max(probe[intersect(candose,1:mtd)])
    deff <- which(probe==meff)
    re   <- min(intersect(candose,deff))
  }

  return(re)
}
