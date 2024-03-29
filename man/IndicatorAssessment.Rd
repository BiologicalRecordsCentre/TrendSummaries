% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IndicatorAssessment.R
\name{IndicatorAssessment}
\alias{IndicatorAssessment}
\title{IndicatorAssessment - Assess the composite trend (indicator)}
\usage{
IndicatorAssessment(
 
    indata = "../outputs/pollinators_2016_arithmetic_logit_occ_composite_trend_iterations.csv",
  output_path = "../outputs/",
  group_name = "pollinators_2016",
  st_length = 5
)
}
\arguments{
\item{indata}{A .csv file containing mean trend (indicator index value) 
values, columns are years, rows are iterations. Columns must be 
ordered left to right, first year to last year. This restriction will
be updated soon.}

\item{output_path}{The location where the output will be saved.}

\item{group_name}{The name used for saving the output}

\item{st_length}{The number of years used for the short-term trend}
}
\value{
A .csv file summarising the long and short-term assessments. The
     "best guess" for the overall trend is taken as the mean value across 
     the iterations. Uncertainty in the assessments are presented as the upper and 
     lower 95% credible intervals, across the iterations.
}
\description{
Many biodiversity indicators require an assessment. 
These assessments typically try to identify if the composite trend
has notably changed over time. This function estimates the proportional
change between the first and last year of the time series (the long-term 
trend), and a user specified short-term trend.  Further changes to this
script will allow users to specify the years used for the short and 
long-term assessment.
}
