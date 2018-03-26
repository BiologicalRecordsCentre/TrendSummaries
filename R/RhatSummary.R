###### Rhat summary ######

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

