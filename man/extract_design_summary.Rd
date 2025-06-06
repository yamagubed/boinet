% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enhanced_functions.R
\name{extract_design_summary}
\alias{extract_design_summary}
\title{Extract Design Parameters Summary}
\usage{
extract_design_summary(boinet_result)
}
\arguments{
\item{boinet_result}{Result object from boinet functions (boinet, tite.boinet,
gboinet, tite.gboinet)}
}
\value{
A tibble with parameter names and values
}
\description{
Extracts design parameters from boinet results in a tidy format.
}
