#' SampPost - Sample posterior function
#' 
#' @description This function is used to extract a number of annual 
#' occupancy estimates from any given posterior. The function loops 
#' through Bugs output files within a given location.
#'
#' @param indata The file path to a location containing .rdata files
#'      of the bugs outputs, one for each species
#' @param output_path The location where the output will be saved.
#' @param REGION_IN_Q The region for which occupancy estimates will be 
#'      extracted. UK based occupancy model examples include,
#'		  'psi.fs', 'psi.fs.r_GB', 'psi.fs.r_ENGLAND'.
#' @param sample_n The number of values extracted from the posterior.
#' @param combined_output This specifies whether the output should be a 
#'      single .csv, containing all species (TRUE) or a single .csv file 
#'      per species (FALSE). Default is TRUE.
#'
#' @return A .csv file for each species containing annual occupancy 
#'       estiamtes for each year as columns, and an iteration and species 
#'       name column.
#' @export

SampPost3 <- function(indata = "../data/model_runs/", 
                      spp.list,
                      output_path = "../data/sampled_posterior_1000/",
                      REGION_IN_Q = "psi.fs",
                      sample_n = 1000,
                      combined_output = TRUE){
  
  ### set up species list we want to loop though ###
  #spp.list <- list.files(indata)[grepl(".rdata", list.files(indata))] # species for which we have models
  
  samp_post <- NULL # create the stacked variable, will be used if combined_output is TRUE.
  
  # loop through species
  for (i in spp.list){
    print(i)
    out <- NULL
    raw_occ <- NULL
    load(paste(indata, i, sep = ""))
    raw_occ <- data.frame(out$BUGSoutput$sims.list[REGION_IN_Q])
    raw_occ <- raw_occ[sample(1:nrow(raw_occ), sample_n),]
    colnames(raw_occ) <- paste("year_", out$min_year:out$max_year, sep = "")
    raw_occ$iteration <- 1:sample_n
    raw_occ$species <- i
    
    if(combined_output == TRUE) {
      row.names(raw_occ) = NULL
      samp_post <- rbind(samp_post, raw_occ)
    } else {
      write.csv(raw_occ, file = paste(output_path, gsub(".rdata", "" ,i), "_sample_", sample_n, "_post_", REGION_IN_Q, ".csv", sep = ""), row.names = FALSE)
    }  
  }
  if(combined_output == TRUE){
    save(samp_post, file = paste(output_path, "all_spp_sample_", sample_n, "_post_", REGION_IN_Q, ".rdata", sep = ""))
  }
}

