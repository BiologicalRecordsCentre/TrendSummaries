FindMinMaxYear <- function(indata = 'outputs_to_analyse/', 
                           output_path = "../data/sampled_posterior_1000/"
                           ){
        
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata)[grepl(".rdata", list.files(indata))] # species for which we have models
  minyear <- NULL
  maxyear <- NULL
  minyears <- NULL
  maxyears <- NULL
  
  # loop through species
  for (i in spp.list){
    print(i)
    out <- NULL
    load(paste(indata, i, sep = ""))
    minyear <- c(minyear, out$min_year)
    maxyear <- c(maxyear, out$max_year)
  }

  print(min(minyear))
  print(max(maxyear))
  print(unique(minyear))
  print(unique(maxyear))
  
  ret_obj <- c(minyear = min(minyear), maxyear = max(maxyear))
  return(ret_obj)

}
