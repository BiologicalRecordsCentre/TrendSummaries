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

SampPost2 <- function(indata = "../data/model_runs/", 
                     output_path = "../data/sampled_posterior_1000/",
                     REGION_IN_Q = "psi.fs",
                     years = minmaxyear,
                     sample_n = 2,
                     combined_output = TRUE){
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata)[grepl(".rdata", list.files(indata))] # species for which we have models
 
  # create empty dataframe of required size
  samp_post <- data.frame(matrix(NA, 
                                 nrow = length(spp.list) * sample_n, 
                                 ncol = (minmaxyear["maxyear"] - minmaxyear["minyear"])+1))
  colnames(samp_post) <- years["minyear"]:years["maxyear"]
  samp_post$iteration <- rep(1:sample_n, length(spp.list))
  samp_post$species <- rep(spp.list, rep(sample_n, length(spp.list)))

  # loop through species
  for (i in spp.list){
    print(i)
    out <- NULL
    raw_occ <- NULL
    load(paste(indata, i, sep = ""))
    raw_occ <- data.frame(out$BUGSoutput$sims.list[REGION_IN_Q])
    raw_occ <- raw_occ[sample(1:nrow(raw_occ), sample_n),]

    # put output into samp_post dataframe
    samp_post[samp_post$species == i, as.character(c(out$min_year:out$max_year))] <- raw_occ
      
    if(combined_output == FALSE) {
      write.csv(raw_occ, file = paste(output_path, gsub(".rdata", "" ,i), "_sample_", sample_n, "_post_", REGION_IN_Q, ".csv", sep = ""), row.names = FALSE)
    }  
  }
  if(combined_output == TRUE){ # can get rid of this line?
    colnames(samp_post) <- c(paste('year_', years["minyear"]:years["maxyear"], sep = ''), 'iteration', 'species')
    save(samp_post, file = paste(output_path, "all_spp_sample_", sample_n, "_post_", REGION_IN_Q, ".rdata", sep = ""))
  }
}

