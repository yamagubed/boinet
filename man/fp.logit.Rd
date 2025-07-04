% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fp.logit.R
\name{fp.logit}
\alias{fp.logit}
\title{Fractional Polynomial Logistic Regression for Dose-Efficacy Modeling}
\usage{
fp.logit(obs, n, dose)
}
\arguments{
\item{obs}{Numeric vector of the number of patients experiencing the event of interest
or normalized equivalent efficacy score at each dose level.}

\item{n}{Numeric vector of the total number of patients treated at each dose level.
Must be positive integers. Length must match \code{obs} and \code{dose}.}

\item{dose}{Numeric vector of dose levels investigated. Should be positive values
representing actual doses (not dose level indices). Length must match \code{obs}
and \code{n}.}
}
\value{
A numeric vector of estimated efficacy probabilities for each dose level,
corresponding to the input \code{dose} vector. Values are bounded between 0 and 1.
The estimates are derived from the best-fitting fractional polynomial model
selected based on deviance criteria.
}
\description{
Performs fractional polynomial (FP) logistic regression with two degrees of
freedom to estimate the efficacy probabilities. This method provides a flexible
alternative to standard polynomial regression for capturing non-linear
dose-efficacy relationships. Fractional polynomials have steadily gained
popularity as a tool for flexible parametric modeling of regression relationships
and are particularly useful when the relationship between dose and response may not
follow simple linear or quadratic patterns.
}
\details{
Fractional polynomials extend conventional polynomials by allowing non-integer powers
from a predefined set. All commonly used transformations such as
the logarithmic, square, cubic, or reciprocal are embedded in the FP method.
This approach is especially valuable in dose-finding studies where the true shape of
the dose-response curve is unknown and may exhibit complex non-linear behavior.

\strong{Mathematical Framework:}
The fractional polynomial of degree m for a positive variable x takes the form:
\deqn{FP_m(x) = \beta_0 + \sum_{j=1}^{m} \beta_j H_j(x)}

where \eqn{H_j(x)} represents transformed versions of x using powers from the set
\{-2, -1, -0.5, 0, 0.5, 1, 2, 3\}, with 0 representing the natural logarithm.

\strong{Implementation Details:}
This function implements FP logistic regression with 2 degrees of freedom,
using very liberal selection criteria (select=0.99999, alpha=0.99999) to avoid
the closed testing procedure and focus on finding the best-fitting model.
}
\examples{
# Modeling efficacy probabilities in a dose-escalation study
dose_levels <- c(25, 50, 100, 200, 400)  # mg doses
efficacy_responses <- c(1, 3, 8, 12, 10)  # patients with efficacy
total_patients <- c(6, 6, 12, 15, 12)     # total patients per dose

# Fit fractional polynomial model
efficacy_probs <- fp.logit(obs = efficacy_responses,
                          n = total_patients,
                          dose = dose_levels)

# Display results
results <- data.frame(
  Dose = dose_levels,
  Observed_Rate = efficacy_responses / total_patients,
  FP_Predicted = round(efficacy_probs, 3)
)
print(results)

}
\references{
\itemize{
\item Royston, P., & Altman, D. G. (1994). Regression using fractional polynomials
of continuous covariates: parsimonious parametric modelling. \emph{Journal of
the Royal Statistical Society: Series C (Applied Statistics)}, 43(3), 429-467.
}
}
\seealso{
\code{\link[mfp]{mfp}} for the underlying fractional polynomial fitting algorithm,
\code{\link{glm}} for standard logistic regression,
\code{\link{obd.select}} for optimal biological dose selection using the output
from this function.
}
\keyword{clinical-trials}
\keyword{dose-response}
\keyword{fractional-polynomial}
\keyword{regression}
