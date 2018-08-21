#' SppTrendSummary - Species trend summary
#' 
#' @description This function extracts trends for individual species
#' from occupancy model outputs. Data describing the uncertainty and 
#' value of the trend estimates are also extracted. Trends are estimated 
#' as the annual growth rate between two given years, for example for a 
#' long-term trend, the function estiamtes annual growth rate between the 
#' first and last year of the time series. This function will be updated 
#' to allow the user to specify the exact years for which the trend is 
#' calculated.
#'
#' @param indata The file path to an selection of .csv files (one per species)
#'      containing sampled annual occupancy estiamtes from the posterior 
#'      distribution. These data can be extract using the {"SampPost"} function.
#' @param output_path The location where the outputs should be saved.
#' @param short_term_length The number of years used for the short-term trend.
#' @param a_17 Are these species "article 17 species? This has specific relevence
#'      to UK biodiversity indicators and will eventually be removed from this 
#'      package.
#' @param include_rhat_check Loads in the {RhatSummary} output data to test if
#'      occupancy estiamtes for the years of interest converged, if not this test
#'      is failed.
#' @param include_prec_check A simple test of whether the precision of the trend
#'      estimate is less than 0.2, if so the fail criterion is true.
#' @param REGION_IN_Q The region for which the trends will be extracted. UK based
#'      occupancy model examples include,
#'		  'psi.fs', 'psi.fs.r_GB', 'psi.fs.r_ENLGAND'.
#' @param model_useable_columns Include a simple column that identifies if the
#'      species failed either the precision or Rhat tests.
#'		  
#'		  
#' @return A summary file (.csv) containing the individual trends for each species. 
#'       Each row is a species. There are trends for the long-term (LT) and 
#'       short-term trend (ST). As the function summarises the trends across a
#'       given number of iterations (based on the SampPost, sample number), the
#'       mean and median are used to produce the best guess of the true trend.
#'       Uncertainty is represented by the 95% credible intervals, and the precision
#'       of the trend estimate. Finally the number of samples used to produce the 
#'       trend are displayed.
#'       
#' @export

SppTrendSummary <- function(indata = "../data/sampled_posterior_1000/", 
                     output_path = "../trend_summary/",
                     short_term_length = 10,
					           a_17 = "no",
					           include_rhat_check = "yes",
					           include_prec_check = "yes",
					           REGION_IN_Q = "psi.fs",
					           model_useable_columns = "yes"
					           ){
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata) # species for which we have models
  
  # annual growth rate function #
  annual_growth_rate <- function(first, last, nyr){
    (((last/first)^(1/nyr))-1)*100
  }
  
  # load a name matching file 
  if (a_17 == "yes"){
	  name_match <- read.csv("../data/spmod.csv", header = TRUE)
  }
  
  final_summary <- NULL
  
  for (i in spp.list){
    temp_data <- NULL
    temp_data <- read.csv(paste(indata, i, sep = ""), header = TRUE)
    
    # change name to latin name #
    spp_name <- as.character(unique(temp_data$species))
    
    # save species name #
    spp_name <- gsub(".rdata", "", spp_name)
	  rhat_name <- spp_name
	    
	  if (a_17 == "yes"){
		  spp_name <- as.character(name_match[name_match$NAME_USED == spp_name, "SPECIES_NAME"])
	  }
    
    temp_data <- temp_data[,1:(ncol(temp_data)-2)]
    
    ## base min and max year on external source ##
    if (a_17 == "yes"){
      yr_sel <- read.csv("../data/article 17 species year selection.csv", header = TRUE)
      min_year <- yr_sel[yr_sel$spp_name == spp_name, "min_year"]
      max_year <- yr_sel[yr_sel$spp_name == spp_name, "max_year"]
      
    } else {
      min_year <- as.numeric(gsub("year_", "", names(temp_data[1])))
      max_year <- as.numeric(gsub("year_", "", names(temp_data[ncol(temp_data)])))
    }
    
    ## estimate long-term annual growth rate ##
    long_term_gr <- annual_growth_rate(
      first = temp_data[,paste("year_", min_year, sep = "")],  # CHAGNE THIS BASED ON VALUES ABOVE!!!!----
      last = temp_data[,paste("year_", max_year, sep = "")],
      nyr = max_year - min_year
    )
    
    #long_term_gr <- annual_growth_rate(
    #  first = temp_data[, "year_1970"],
    #  last = temp_data[, "year_1997"],
    #  nyr = max_year - min_year
    #)
    
    # estimate short-term annual growth rate #
    st_first <- max_year-short_term_length # extract first year for the 10 year trend
    
    short_term_gr <- annual_growth_rate(
      first = temp_data[, paste("year_", st_first, sep = "")],
      last = temp_data[, paste("year_", max_year, sep = "")],
      nyr = max_year - st_first
    )
    
    #short_term_gr <- annual_growth_rate(
    #  first = temp_data[, "year_1987"],
    #  last = temp_data[, "year_1997"],
    #  nyr = max_year - st_first
    #)
    
    # summarise across the iterations using median and 95% CI #
    # drop NaN, NA and infs #
    long_term_gr <- long_term_gr[!is.na(long_term_gr)]
    long_term_gr <- long_term_gr[!is.infinite(long_term_gr)]
    short_term_gr <- short_term_gr[!is.na(short_term_gr)]
    short_term_gr <- short_term_gr[!is.infinite(short_term_gr)]
    
    temp_spp_res <- NULL
    temp_spp_res <- data.frame(spp_name, min_year, max_year, st_first, 
                               median_LT_trend = median(long_term_gr),
                               mean_LT_trend = mean(long_term_gr),
                               LT_trend_precision = 1/(sd(long_term_gr)^2),
                               LT_trend_lower_2.5CI = quantile(long_term_gr, prob = 0.025), 
                               LT_trend_upper_97.5CI = quantile(long_term_gr, prob = 0.975),
                               LT_iterations_used = length(long_term_gr),
                               median_ST_trend = median(short_term_gr),
                               mean_ST_trend = mean(short_term_gr), 
                               ST_trend_precision = 1/(sd(short_term_gr)^2),
                               ST_trend_lower_2.5CI = quantile(short_term_gr, prob = 0.025), 
                               ST_trend_upper_97.5CI = quantile(short_term_gr, prob = 0.975),
                               ST_iterations_used = length(short_term_gr),
                               row.names = NULL)
    
    # add the check columns #
    temp_rhat_data <- NULL
    if (include_rhat_check == "yes"){
      load("../rhat_summary/article_17_psi_fs.rdata")
      temp_rhat_data <- rhat_save[rhat_save$spp == rhat_name & rhat_save$region == REGION_IN_Q,]
      temp_spp_res$LT_rhat_fail <- sum(temp_rhat_data[temp_rhat_data$year %in% c(min_year, max_year), "Rhat"] > 1.1) > 0
      temp_spp_res$ST_rhat_fail <- sum(temp_rhat_data[temp_rhat_data$year %in% c(st_first, max_year), "Rhat"] > 1.1) > 0
      
      # if an rhat is na set it as a fail
      if(is.na(temp_spp_res$LT_rhat_fail)){
        temp_spp_res$LT_rhat_fail <- TRUE
      }
      
      if(is.na(temp_spp_res$ST_rhat_fail)){
        temp_spp_res$ST_rhat_fail <- TRUE
      }
        
    }
    
    if (include_prec_check == "yes"){
      temp_spp_res$LT_prec_fail <- temp_spp_res$LT_trend_precision < 0.2
      temp_spp_res$ST_prec_fail <- temp_spp_res$ST_trend_precision < 0.2
    }
    
    # add model useable columns, if either the prec or rhat fails the model is not useable
    if (model_useable_columns == "yes"){
      
      
      temp_spp_res$LT_model_useable <- "yes"
      if(sum(temp_spp_res$LT_rhat_fail, temp_spp_res$LT_prec_fail) > 0){
        temp_spp_res$LT_model_useable <- "no"
      }
     
      temp_spp_res$ST_model_useable <- "yes"
      if(sum(temp_spp_res$ST_rhat_fail, temp_spp_res$ST_prec_fail) > 0){
        temp_spp_res$ST_model_useable <- "no"
      }   
     
    }
    
    final_summary <- rbind(final_summary, temp_spp_res) # inefficient, change this later #
    
    ## add trend histogram ##
    
  }
  
  # save the final summary data frame #
  write.csv(final_summary, file = paste(output_path, "trend_summary.csv", sep = ""), row.names = FALSE)
}

  