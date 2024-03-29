% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampPost.R
\name{SampPost}
\alias{SampPost}
\title{SampPost - Sample posterior function}
\usage{
SampPost(
  indata = "../data/model_runs/",
  output_path = "../data/sampled_posterior_1000/",
  REGION_IN_Q = "psi.fs",
  sample_n = 1000,
  group_name = "",
  combined_output = TRUE,
  max_year_model = NULL,
  min_year_model = NULL,
  write = FALSE
)
}
\arguments{
\item{indata}{The file path to a location containing .rdata files
of the bugs outputs, one for each species}

\item{output_path}{The location where the output will be saved.}

\item{REGION_IN_Q}{The region for which occupancy estimates will be 
extracted. UK based occupancy model examples include,
 'psi.fs', 'psi.fs.r_GB', 'psi.fs.r_ENGLAND'.}

\item{sample_n}{The number of values extracted from the posterior.}

\item{group_name}{The name of the species group we are running, used for
naming output files.}

\item{combined_output}{This specifies whether the output should be a 
single .csv, containing all species (TRUE) or a single .csv file 
per species (FALSE). Default is TRUE. If TRUE all outputs 
must have the same start and end year. To combine outputs run 
over different year ranges please use \code{SampBind()}.}

\item{max_year_model}{The last year for which estimates are available
in the posterior. If this is available in the metadata (models
run using sparta >= 0.2.06) then it will be added automatically.
 If this is not available then any input value for this argument
 will be used or this will default to NULL.}

\item{min_year_model}{The first year for which estimates are available
in the posterior. If this is available in the metadata (models
run using sparta >= 0.2.06) then it will be added automatically.
 If this is not available then any input value for this argument
 will be used or this will default to NULL.}

\item{write}{Boolean. If write = TRUE and combine_outputs = TRUE then then the .rdata 
files are written to output_path.}
}
\value{
A .csv file for each species containing annual occupancy 
      estimates for each year as columns, and an iteration and species 
      name column. If combined_output = TRUE the results are saved as
      an rdata file instead.
}
\description{
This function is used to extract a number of annual 
occupancy estimates from any given posterior produced using the 
\code{occDetFunc()} in the package sparta. The function loops 
through BUGS output files within a given location.
}
