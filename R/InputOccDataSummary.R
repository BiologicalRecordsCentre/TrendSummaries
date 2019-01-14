#' InputOccDataSummary
#' 
#' @description THIS FUNCTION IS STILL UNDER REVIEW - This function takes .rdata 
#' files created by the Data_cleaning.function and calculates how many species 
#' have records in each region follwoing the data cleaning process. Furthermore, 
#' this function can be used to extract the first and last year of data for each 
#' species following the data cleaning process.
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
#' @param species_year_summary Do you want to save a file that extracts the year 
#' of the first and last record for the each species. This is calculated post 
#' occupancy model filtering, e.g. site with < X years dropped.
#'  
#' @return A .csv file summarising the number of species included for each
#' region from the cleaned dataset
#' 
#' @export

InputOccDataSummary <- function(indata = "data/model_runs/", 
                    output_path = "output/", 
                    minNyear = 2,
                    minNrecs = 50,
                    save_name = "cirrus",
                    outhwaite_spp_clean = TRUE,
                    species_year_summary = TRUE){
  
  # for testing #
  # indata = "W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/1. Data/Cleaned Datasets/"
  # minNyear = 2
  
  file_list <- list.files(indata)[grep("Cleaned_Data.rdata", list.files(indata))] # create a list of cleaned .rdata files to loop through.
  
  # TEMPORARY - drop taxa not in Charlie's paper #
  file_list <- file_list[!file_list %in% c("Fish_170809_Cleaned_Data.rdata", "HypogeanCrustacea_170809_Cleaned_Data.rdata")]
  
  # How many species in each region
  cn_id <- read.csv("data/sq1km_country_id_UKonly_bordercells_tosmallercountry.csv", header = TRUE)
  cn_id[is.na(cn_id)] <- 0
  
  regional_data_summary <- NULL
  spp_year_summary <- NULL
  spp_rec_summary <- NULL
  
  # loop through cleaned group .rdata files.
  for (i in file_list){
    taxa_data <- NULL
    load(paste(indata, i, sep = ""))
    
    # drop sites with less than X (2 buy default) number of years of data
    if(i == "Moths_2017_02_14_Cleaned_Data.rdata"){
      names(taxa_data) <- gsub("SQ_1KM", "TO_GRIDREF", names(taxa_data))
      taxa_data$YEAR <-as.numeric(format(taxa_data$TO_STARTDATE, "%Y")) # add year column to the moths data
    }
    
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
    
    ### extract the first and last record for each species ###
    if(species_year_summary == TRUE){
      min_year <- data.frame(aggregate(.~CONCEPT, data=taxa_data[, c("CONCEPT","YEAR")], min))
      names(min_year)[2]<-"min_year"
      max_year <- data.frame(aggregate(.~CONCEPT, data=taxa_data[, c("CONCEPT","YEAR")], max))
      names(max_year)[2]<-"max_year"
      min_max <- merge(min_year, max_year)
      min_max$group <- group
      spp_year_summary <- rbind(spp_year_summary, min_max)
    }
    
    # create mini datasets for each region #
    ENGLAND_taxa_data <- taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$ENGLAND == 1, "SQ1_SQUARE"]),]
    SCOTLAND_taxa_data <- taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$SCOTLAND == 1, "SQ1_SQUARE"]),]
    WALES_taxa_data <- taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$WALES == 1, "SQ1_SQUARE"]),]
    NI_taxa_data <- taxa_data[taxa_data$TO_GRIDREF %in% as.character(cn_id[cn_id$NORTHERN_IRELAND == 1, "SQ1_SQUARE"]),]
    
    # extract records per species per region #
    ENGLAND_taxa_table <- data.frame(table(ENGLAND_taxa_data$CONCEPT))
    names(ENGLAND_taxa_table) <- c("species", "ENGLAND_recs")
    SCOTLAND_taxa_table <- data.frame(table(SCOTLAND_taxa_data$CONCEPT))
    names(SCOTLAND_taxa_table) <- c("species", "SCOTLAND_recs")
    WALES_taxa_table <- data.frame(table(WALES_taxa_data$CONCEPT))
    names(WALES_taxa_table) <- c("species", "WALES_recs")
    NI_taxa_table <- data.frame(table(NI_taxa_data$CONCEPT))
    names(NI_taxa_table) <- c("species", "NI_recs")
    
    # join the species record counts per region into a single table
    group_spp_rec_summary <- merge(ENGLAND_taxa_table, SCOTLAND_taxa_table)
    group_spp_rec_summary <- merge(group_spp_rec_summary, WALES_taxa_table)
    group_spp_rec_summary <- merge(group_spp_rec_summary, NI_taxa_table)
    group_spp_rec_summary$group <- group
    
    spp_rec_summary <- rbind(spp_rec_summary, group_spp_rec_summary)
  
  }

  # save the files of interest #
  names(spp_year_summary)[1] <- "spp_name"
  write.csv(regional_data_summary, file = paste(output_path, save_name, "_regional_data_summary.csv", sep = ""), row.names = FALSE)
  write.csv(spp_year_summary, file = paste(output_path, save_name, "_species_year_summary.csv", sep = ""), row.names = FALSE)
  write.csv(spp_rec_summary, file = paste(output_path, save_name, "_spp_rec_summary.csv", sep = ""), row.names = FALSE)
  
  #return(regional_data_summary)
}
