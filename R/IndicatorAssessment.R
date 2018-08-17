#' IndicatorAssessment - Assess the composite trend (indicator)
#' 
#' @description Many biodiversity indicators require an assessment. 
#' These assessments typically try to identify if the composite trend
#' has notably changed over time. This function estimates the proportional
#' change between the first and last year of the time series (the long-term 
#' trend), and a user specified short-term trend.  Further changes to this
#' script will allow users to specify the years used for the short and 
#' long-term assessment.
#'
#' @param indata A .csv file containing mean trend (indicator index value) 
#'      values, columns are years, rows are iterations. Columns must be 
#'      ordered left to right, first year to last year. This restriction will
#'      be updated soon.
#' @param output_path The location where the output will be saved.
#' @param group_name The name used for saving the output
#' @param st_length The number of years used for the short-term trend
#'
#' @return A .csv file summarising the long and short-term assessments. The
#'      "best guess" for the overall trend is taken as the mean value across 
#'      the iterations. Uncertainty in the assessments are presented as the upper and 
#'      lower 95% credible intervals, across the iterations.
#'      
#' @export

IndicatorAssessment <- function(indata = "../outputs/pollinators_2016_arithmetic_logit_occ_composite_trend_iterations.csv", 
                            output_path = "../outputs/",
                            group_name = "pollinators_2016",
                            st_length = 5
                            ){
  
  ### load the iterations of the indicator data ###
  iter_data <- read.csv(indata, header = TRUE)
  
  # calculate the long-term trend across each iteration 
  LT_prop_change_iters <- (iter_data[,ncol(iter_data)] - iter_data[,1])/iter_data[,1]
  ST_prop_change_iters <- (iter_data[,ncol(iter_data)] - iter_data[,(ncol(iter_data)-st_length)])/iter_data[,(ncol(iter_data)-st_length)]
  
  # summarise these using mean and 95% CI
  ind_assessment <- data.frame(trend = c("Long-term", "Short-term"), 
             mean_trend = c(mean(LT_prop_change_iters), mean(ST_prop_change_iters)),
             lower_CI = c(quantile(LT_prop_change_iters, prob = 0.05), quantile(ST_prop_change_iters, prob = 0.05)),
             upper_CI = c(quantile(LT_prop_change_iters, prob = 0.95), quantile(ST_prop_change_iters, prob = 0.95)),
             first_year = c(names(iter_data)[1], names(iter_data)[(ncol(iter_data)-st_length)]),
             last_year = c(names(iter_data)[ncol(iter_data)], names(iter_data)[ncol(iter_data)])
  )
  
  write.csv(ind_assessment, file = paste(output_path, group_name, "_ind_assessment.csv", sep = ""), row.names = FALSE)
}
