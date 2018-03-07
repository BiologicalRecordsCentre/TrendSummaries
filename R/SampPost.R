###### Sample posterior function ######

SampPost <- function(indata = "../data/model_runs/", 
                    output_path = "../data/sampled_posterior_1000/",
                    REGION_IN_Q = "psi.fs.r_GB",
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

