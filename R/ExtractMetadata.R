#'ExtractMetadata - extract metadata required for filtering from occupancy model outputs
#'
#' @description This function extracts the metadata from occupancy models
#'              run using sparta >= 0.2.06. This then allows filtering based
#'              on record number, year gaps and first and last records.
#'              
#' @param indata The file path to a location containing .rdata files
#'      of the bugs outputs, one for each species. These must have
#'      been created with sparta version >= 0.2.06.
#' @param regions A vector of region names for which metadata is require
#'      (e.g. \code{c("region1","region2")}).
#'      The default is NULL and will only return metadata for the whole
#'      model.
#'
#' @return A data frame containing the metadata for each species for which an
#'         output was available.
#'     \itemize{
#'       \item{Species - the name of the species as saved in the model output.}
#'       \item{n_obs - the number of observations of the foacl species on which 
#'        the model is based.}
#'       \item{min_year_data - the year of the first observation of the focal
#'        species.}
#'       \item{max_year_data - the year of the last observation of the focal
#'        species.}
#'       \item{gap_start - number of consecutive years from the start of the
#'        time series that have no observations of the focal species.}
#'       \item{gap_end - number of consecutive years from the end of the
#'        time series that have no observations of the focal species.}
#'       \item{gap_middle - number of consecutive years in the middle (after
#'         the first observation but before the last) of the time series
#'         with no observations of the focal species.}}
#' If regions are provided the same summary information is 
#'      calculated for each region.
#' @export
#' @importFrom plyr rbind.fill      

ExtractMetadata <- function(indata = "../data/model_runs/", 
                            regions = NULL){
  
  # get the species list
  spp.list <- list.files(indata, pattern = "*.rdata") 
  
  # set up a dataframe
  templatedf <- data.frame(Species = as.character(), n_obs = as.numeric(), min_year_data = as.integer(),
                           max_year_data = as.integer(), min_year_model = as.integer(),
                           max_year_model = as.integer(), gap_start = as.integer(), gap_end = as.integer(),
                           gap_middle = as.integer())
  
  # adjust template if regional
  if(!is.null(regions)){
    rMD_tp_colnames <- c("n_obs_r_", "max_year_data_r_", "min_year_data_r_", "gap_start_r_", 
                  "gap_end_r_", "gap_middle_r_")
    rMD_tp_cols <- do.call(paste0,expand.grid(rMD_tp_colnames,regions))
    rMD_template <- setNames(data.frame(matrix(ncol = length(rMD_tp_cols), nrow = 0)), rMD_tp_cols)
    templatedf <- cbind(templatedf, rMD_template)
    }
  
  # loop through species
  for (i in spp.list){
    print(i)
    out <- NULL
    load(paste0(indata, i))
    cur_attr <- attributes(out) # get the metadata
    
    # extract overall MD
    cur_MD <- data.frame(Species = out$SPP_NAME, 
                         n_obs = cur_attr$metadata$analysis$summary$n_obs,
                         min_year_data = cur_attr$metadata$analysis$summary$min_year_data,
                         max_year_data = cur_attr$metadata$analysis$summary$max_year_data,
                         min_year_model = cur_attr$metadata$analysis$summary$min_year_model,
                         max_year_model = cur_attr$metadata$analysis$summary$max_year_model,
                         gap_start = cur_attr$metadata$analysis$summary$gap_start,
                         gap_end = cur_attr$metadata$analysis$summary$gap_end,
                         gap_middle = cur_attr$metadata$analysis$summary$gap_middle)
    
    if(!is.null(regions)){
      
      # extract region MD and restrict to user requested and available
      r_n_obs <- as.data.frame(cur_attr$metadata$analysis$summary$region_nobs)
      r_n_obs <- r_n_obs[,which(names(r_n_obs) %in% paste0("n_obs_r_",regions))]
      r_years <- as.data.frame(cur_attr$metadata$analysis$summary$region_years)
      r_years_max <- r_years[,which(names(r_years) %in% paste0("max_year_data_r_",regions))]
      r_years_min <- r_years[,which(names(r_years) %in% paste0("min_year_data_r_",regions))]
      r_years_sg <- r_years[,which(names(r_years) %in% paste0("gap_start_r_",regions))]
      r_years_eg <- r_years[,which(names(r_years) %in% paste0("gap_end_r_",regions))]
      r_years_mg <- r_years[,which(names(r_years) %in% paste0("gap_middle_r_",regions))]
      
      # bind into one
      rMD_vec <- cbind(r_n_obs, r_years_max, r_years_min, r_years_sg, r_years_mg, r_years_eg)
      cur_MD <- cbind(cur_MD, rMD_vec)
    }
    templatedf <- rbind.fill(templatedf, cur_MD)
  }
  return(templatedf)
}