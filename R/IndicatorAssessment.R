###### composite_trend_assessment ######

# clear R

# I will update this to make this flexible to different inputs and allow the user to specify the years etc... #

IndicatorAssessment <- function(indata = "../outputs/pollinators_2016_arithmetic_logit_occ_composite_trend_iterations.csv", 
                            output_path = "../outputs/",
                            group_name = "pollinators_2016"
                            ){
  
  ### load the iterations of the indicator data ###
  iter_data <- read.csv(indata, header = TRUE)
  
  # calculate the long-term trend across each iteration 
  LT_prop_change_iters <- (iter_data[,ncol(iter_data)] - iter_data[,1])/iter_data[,1]
  ST_prop_change_iters <- (iter_data[,ncol(iter_data)] - iter_data[,(ncol(iter_data)-5)])/iter_data[,(ncol(iter_data)-5)]
  
  # summarise these using mean and 95% CI
  ind_assessment <- data.frame(trend = c("Long-term", "Short-term"), 
             mean_trend = c(mean(LT_prop_change_iters), mean(ST_prop_change_iters)),
             lower_CI = c(quantile(LT_prop_change_iters, prob = 0.05), quantile(ST_prop_change_iters, prob = 0.05)),
             upper_CI = c(quantile(LT_prop_change_iters, prob = 0.95), quantile(ST_prop_change_iters, prob = 0.95)),
             first_year = c(names(iter_data)[1], names(iter_data)[(ncol(iter_data)-5)]),
             last_year = c(names(iter_data)[ncol(iter_data)], names(iter_data)[ncol(iter_data)])
  )
  
  write.csv(ind_assessment, file = paste(output_path, group_name, "_ind_assessment.csv", sep = ""), row.names = FALSE)
}
