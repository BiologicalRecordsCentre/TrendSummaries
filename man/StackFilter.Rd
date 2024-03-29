% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StackFilter.R
\name{StackFilter}
\alias{StackFilter}
\title{StackFilter - clip and filter stacked estimates produced by SampBind}
\usage{
StackFilter(
  input = "memory",
  indata,
  output_path,
  group_name,
  metadata,
  region = NULL,
  minObs = NULL,
  maxStartGap = NULL,
  maxEndGap = NULL,
  maxMiddleGap = NULL,
  keepSpecies = NULL,
  removeSpecies = NULL,
  ClipFirst = FALSE,
  ClipLast = FALSE
)
}
\arguments{
\item{indata}{The file path to the .rdata file produced using \code{SampBind()}.}

\item{output_path}{The file path to the folder in which the filtered stack should be saved.}

\item{group_name}{Name used for the output file}

\item{metadata}{Metadata detailing the information on which filtering is required for each
            species. This is produced using \code{ExtractMetadata()}.
            Metadata can be produced by the user but the required columns must be included:
            \itemize{
   \item{Species - needed for all filters. The name of the species. Must match those in the indata}
   \item{minObs - needs n_obs}
   \item{maxStartGap - needs gap_start}
   \item{maxEndGap - needs gap_end}
   \item{maxMiddleGap - needs middle_gap}
   \item{ClipFirst - needs min_year_model and min_year_data}
   \item{ClipLast - neeeds max_year_model and max_year_data}}
If region is not NULL all columns must be suffixed with "_r_region" e.g. n_obs_r_ENGLAND if the
posterior samples are the values for the region ENGLAND from a model with multiple regions.}

\item{region}{The name of the focal region if estimates are for one region from a regional model.
This must exactly match the region name used in the model.}

\item{minObs}{The threshold number of observations a species must have. Species with fewer observations
are removed by this filter.}

\item{maxStartGap}{The maximum number of years allowed between the first model estimate and the first
observation.}

\item{maxEndGap}{The maximum number of years allowed between the last observation and the last model
estimate.}

\item{maxMiddleGap}{The maximum number of consecutive years allowed between the first and last
observations in which the focal species was not observed.}

\item{keepSpecies}{A vector of species names that match those in indata. All other species will be
removed from the data.}

\item{removeSpecies}{A vector of species names that match those in indata. These species will be
removed from the data.}

\item{ClipFirst}{Whether to clip all species time series to begin in the year of their first observation.}

\item{ClipLast}{Whether to clip all species time series to end in the year of their last observation.}
}
\value{
This function will save an .rdata file containing the stacked estimates after the specified filters.
}
\description{

}
