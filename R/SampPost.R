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
#'		  'psi.fs', 'psi.fs.r_GB', 'psi.fs.r_ENLGAND'.
#' @param sample_n The number of values extracted from the posterior.
#'
#' @return A .csv file for each species containing annual occupancy 
#'       estiamtes for each year as columns, and an iteration and species 
#'       name column.
#' @export

SampPost <- function(indata = "../data/model_runs/", 
                    output_path = "../data/sampled_posterior_1000/",
                    REGION_IN_Q = "psi.fs",
                    sample_n = 1000){
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata)[grepl(".rdata", list.files(indata))] # species for which we have models
  
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
    write.csv(raw_occ, file = paste(output_path, gsub(".rdata", "" ,i), "_sample_", sample_n, "_post_", REGION_IN_Q, ".csv", sep = ""), row.names = FALSE)
  }
}

