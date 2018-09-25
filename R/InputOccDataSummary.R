#' InputOccDataSummary
#' 
#' @description THIS FUNCTION IS STILL UNDER REVIEW - This function takes .rdata 
#' files created by the Data_cleaning.function and calculates how many species 
#' have records in each region follwoing the data cleaning process.
#'
#' @param indata The file path to a folder contain .rdata output files 
#' from the Data_cleaning.function
#' @param output_path The file path for the location where we want to 
#' save the outputs
#' @param minNyear The minimum number of years of records a site needs to be 
#' included in the final modelled dataset
#' @param minNrecs The total (across all regions) minimum number of records a 
#' species needs to be included in the final modelled dataset
#' @param save_name the name of the file to be saved
#' @param outhwaite_spp_clean Set to TRUE or FALSE. ONly include species included 
#' in Outhwaite's data paper
#'  
#' @return A .csv file summarising the number of species included for each
#' region from the cleaned dataset
#' 
#' @export
#' 

InputOccDataSummary <- function(indata = "data/model_runs/", 
                    output_path = "output/", 
                    minNyear = 2,
                    minNrecs = 50,
                    save_name = "cirrus",
                    outhwaite_spp_clean = TRUE){
  
  # for testing #
  # indata = "W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/1. Data/Cleaned Datasets/"
  # minNyear = 2
  
  file_list <- list.files(indata)[grep(".rdata", list.files(indata))] # create a list of cleaned .rdata files to loop through.
  
  # TEMPORARY #
  file_list <- file_list[!file_list %in% c("Moths_2017_02_14_Cleaned_Data.rdata")]
  
  # How many species in each region
  cn_id <- read.csv("data/sq1km_country_id_UKonly_bordercells_tosmallercountry.csv", header = TRUE)
  cn_id[is.na(cn_id)] <- 0
  
  regional_data_summary <- NULL
  
  # loop through cleaned group .rdata files.
  for (i in file_list){
    taxa_data <- NULL
    load(paste(indata, i, sep = ""))
    
    # drop sites with less than X (2 buy default) number of years of data
    visit_data <- unique(taxa_data[, c("TO_GRIDREF", "YEAR")]) # just take the visits (unique combination of sites and years)
    visit_data <- as.data.frame(table(visit_data$TO_GRIDREF)) # extract how many years of data per grid cell
    names(visit_data) <- c("TO_GRIDREF", "nyears") # sort column names
    good_sites <- as.character(visit_data[visit_data$nyears >= minNyear, "TO_GRIDREF"]) # identify which sites have sufficient sampling
    taxa_data <- taxa_data[taxa_data$TO_GRIDREF %in% good_sites,]# drop site falling below the threshold
    
    # drop species with less than X number of records in total
    spp_data <- as.data.frame(table(taxa_data$CONCEPT)) # extract how many records per species
    names(spp_data) <- c("CONCEPT", "nrecs") # sort column names
    good_spp <- as.character(spp_data[spp_data$nrecs >= minNrecs, "CONCEPT"]) # identify which sites have sufficient sampling
    
    # drop species not included in Charlie's data paper - this removes species with taxonomic issues etc...
    if(outhwaite_spp_clean == TRUE){
      cust_spp_clean <- read.csv("W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/6. Indicators and other analyses/Good_sp_list_PAPER.csv", header = TRUE)
      cust_spp_clean <- as.character(cust_spp_clean$SPECIES)
      good_spp <- good_spp[good_spp %in% cust_spp_clean]
    }
    
    taxa_data <- taxa_data[taxa_data$CONCEPT %in% good_spp,] # drop species falling below the threshold

    # Regional number of records
    UK_spp_recs <- length(unique(taxa_data$CONCEPT))
    ENGLAND_spp_recs <- length(unique(taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$ENGLAND == 1, "SQ1_SQUARE"]), "CONCEPT"]))
    SCOTLAND_spp_recs <- length(unique(taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$SCOTLAND == 1, "SQ1_SQUARE"]), "CONCEPT"]))
    WALES_spp_recs <- length(unique(taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$WALES == 1, "SQ1_SQUARE"]), "CONCEPT"]))
    NI_spp_recs <- length(unique(taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$NORTHERN_IRELAND == 1, "SQ1_SQUARE"]), "CONCEPT"]))
    group <- gsub("_Cleaned_Data.rdata", "", i)
    
    regional_data_summary <- rbind(regional_data_summary, data.frame(group,
                                                                     UK_spp_recs,
                                                                     ENGLAND_spp_recs,
                                                                     SCOTLAND_spp_recs,
                                                                     WALES_spp_recs,
                                                                     NI_spp_recs))
    
  }
  
  write.csv(regional_data_summary, file = paste(output_path, save_name, "_regional_data_summary.csv", sep = ""))
  return(regional_data_summary)
  
}