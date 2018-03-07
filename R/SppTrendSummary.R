###### species trend summary ######

SppTrendSummary <- function(indata = "../data/sampled_posterior_1000/", 
                     output_path = "../trend_summary/",
                     short_term_length = 10){
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata) # species for which we have models
  
  # annual growth rate function #
  annual_growth_rate <- function(first, last, nyr){
    (((last/first)^(1/nyr))-1)*100
  }
  
  final_summary <- NULL
  
  for (i in spp.list){
    temp_data <- NULL
    temp_data <- read.csv(paste(indata, i, sep = ""), header = TRUE)
    spp_name <- as.character(unique(temp_data$species))
    temp_data <- temp_data[,1:(ncol(temp_data)-2)]
    min_year <- as.numeric(gsub("year_", "", names(temp_data[1])))
    max_year <- as.numeric(gsub("year_", "", names(temp_data[ncol(temp_data)])))
    
    ## estimate long-term annual growth rate ##
    long_term_gr <- annual_growth_rate(
      first = temp_data[,1],
      last = temp_data[,ncol(temp_data)],
      nyr = max_year - min_year
    )
    
    # estimate short-term annual growth rate #
    st_first <- max_year-short_term_length # extract first year for the 10 year trend
    
    short_term_gr <- annual_growth_rate(
      first = temp_data[, names(temp_data) == paste0("year_", st_first)],
      last = temp_data[,ncol(temp_data)],
      nyr = max_year - st_first
    )
    
    # summarise across the iterations using median and 95% CI #
    # drop NaN, NA and infs #
    long_term_gr <- long_term_gr[!is.na(long_term_gr)]
    long_term_gr <- long_term_gr[!is.infinite(long_term_gr)]
    short_term_gr <- short_term_gr[!is.na(short_term_gr)]
    short_term_gr <- short_term_gr[!is.infinite(short_term_gr)]
    
    
    temp_spp_res <- NULL
    temp_spp_res <- data.frame(spp_name, min_year, max_year, st_first, 
                               LT_trend = median(long_term_gr), 
                               LT_trend_lower_2.5CI = quantile(long_term_gr, prob = 0.025), 
                               LT_trend_upper_97.5CI = quantile(long_term_gr, prob = 0.975),
                               LT_iterations_used = length(long_term_gr),
                               ST_trend = median(short_term_gr),
                               ST_trend_lower_2.5CI = quantile(short_term_gr, prob = 0.025), 
                               ST_trend_upper_97.5CI = quantile(short_term_gr, prob = 0.975),
                               ST_iterations_used = length(short_term_gr),
                               row.names = NULL)
    
    final_summary <- rbind(final_summary, temp_spp_res) # inefficient, change this later #
  }
  
  # save the final summary data frame #
  write.csv(final_summary, file = paste(output_path, "trend_summary.csv", sep = ""), row.names = FALSE)
}

  