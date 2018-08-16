#' RhatSummary - Rhat Summary function
#' 
#' @description This function extracts the rhat values for the annual 
#' occupancy estimates for a set of species for a given region. The 
#' function loops through Bugs output files within a given location.
#'
#' @param indata The file path to a location containing .rdata files
#'      of the bugs outputs, one for each species
#' @param output_path The location where the output will be saved.
#' @param REGION_IN_Q The region for which the rhat values will be 
#'      extracted. UK based occupancy model examples include,
#'		  'psi.fs', 'psi.fs.r_GB', 'psi.fs.r_ENLGAND'.
#' @param group_run The name of the group of species that were run.
#'      This is simply used for naming the save file
#'
#' @return An .rdata file containing the rhat values for all species 
#'      for all years for the region in question.
#' @export

RhatSummary <- function(indata = "../data/model_runs/", 
                     output_path = "../rhat_summary/",
                     REGION_IN_Q = "psi.fs",
                     group_run = "article_17"){
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata) # species for which we have models

  rhat_save <- NULL
  
  for (i in spp.list){
    print(i)
    
    if(grepl(".rdata", i)){
      out <- NULL
      rhat_summary <- NULL
      rhat_temp <- NULL
      load(paste(indata, i, sep = ""))
      rhat_summary <- data.frame(out$BUGSoutput$summary[grep(REGION_IN_Q, row.names(out$BUGSoutput$summary)),])
      rhat_temp <- data.frame(Rhat = rhat_summary$Rhat, 
                            year = rep(out$min_year:out$max_year, (nrow(rhat_summary)/length(out$min_year:out$max_year))),
                            region = gsub("\\[[[:digit:]]{1,}\\]", "", row.names(rhat_summary)),
                            spp = out$SPP_NAME)
    }
    
    if(grepl(".csv", i)){
      rhat_summary <- NULL
      rhat_temp <- NULL
      rhat_summary <- read.csv(paste(indata, i, sep = ""), header = TRUE)
      rhat_summary <- rhat_summary[grep(REGION_IN_Q, rhat_summary$X),]
      n_region <- length(unique(gsub("\\[[[:digit:]]{1,}\\]", "", rhat_summary$X)))
      rhat_temp <- data.frame(Rhat = rhat_summary$PSRF, 
                              year = rep(1970:(1970 + (nrow(rhat_summary)/n_region)-1), n_region),
                              region = gsub("\\[[[:digit:]]{1,}\\]", "", rhat_summary$X),
                              spp = substr(i, 1, (regexpr("_it",i)[1]-1)))
    }
    
    rhat_save <- rbind(rhat_save, rhat_temp)
  }
  
  save(rhat_save, file = paste(output_path, group_run, "_", gsub(".", "_", REGION_IN_Q, fixed = TRUE), ".rdata", sep = ""))
}

