
#' Utility defined by weighted function
#'
#' Given estimated toxicity and efficacy probabilities, the utility which is
#' defined by a weighted function is Calculated.
#' @param probt Estimated toxicity probability
#' @param probe Estimated efficacy probability
#' @param w1 Weight for toxicity-efficacy trade-off (default: 0.33)
#' @param w2 Weight for penalty imposed on toxic doses (default: 1.09)
#' @param tox.upper Upper bound of toxicity probability (default: 0.462)
#' @return Utility value
#' @examples
#' utility.weighted(probt=c(0.1,0.2,0.4),probe=c(0.2,0.6,0.6));
#' @export

utility.weighted <- function(probt,probe,
                             w1=0.33,w2=1.09,tox.upper=0.462)
{
  return(probe-w1*probt-w2*probt*(probt>tox.upper))
}

