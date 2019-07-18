#'StackFilter - clip and filter stacked estimates produced by SampBind
#'
#'@description 
#'
#'@param indata The file path to the .rdata file produced using \code{SampBind()}.
#'@param output_path The file path to the folder in which the filtered stack should be saved.
#'@param group_name Name used for the output file
#'@param metadata Metadata detailing the information on which filtering is required for each
#'                species. This is produced using \code{ExtractMetadata()}.
#'                Metadata can be produced by the user but the required columns must be included:
#'                \itemize{
#'       \item{Species - needed for all filters. The name of the species. Must match those in the indata}
#'       \item{minObs - needs n_obs}
#'       \item{maxStartGap - needs gap_start}
#'       \item{maxEndGap - needs gap_end}
#'       \item{maxMiddleGap - needs middle_gap}
#'       \item{ClipFirst - needs min_year_model and min_year_data}
#'       \item{ClipLast - neeeds max_year_model and max_year_data}}
#'    If region is not NULL all columns must be suffixed with "_r_region" e.g. n_obs_r_ENGLAND if the
#'    posterior samples are the values for the region ENGLAND from a model with multiple regions.
#' @param region The name of the focal region if estimates are for one region from a regional model.
#'               This must exactly match the region name used in the model.
#' @param minObs The threshold number of observations a species must have. Species with fewer observations
#'               are removed by this filter.
#' @param maxStartGap The maximum number of years allowed between the first model estimate and the first
#'               observation.
#' @param maxEndGap The maximum number of years allowed between the last observation and the last model
#'               estimate.
#' @param maxMiddleGap The maximum number of consecutive years allowed between the first and last
#'                  observations in which the focal species was not observed.
#' @param keepSpecies A vector of species names that match those in indata. All other species will be
#'                removed from the data.
#' @param removeSpecies A vector of species names that match those in indata. These species will be
#'                removed from the data.
#' @param ClipFirst Whether to clip all species time series to begin in the year of their first observation.
#' @param ClipLast Whether to clip all species time series to end in the year of their last observation.
#'   
#'@return This function will save an .rdata file containing the stacked estimates after the specified filters.
#'
#'@export
#'

StackFilter <- function(indata, 
                        output_path,
                        group_name,
                        metadata,
                        region = NULL,
                        minObs = NULL,
                        maxStartGap = NULL,
                        maxEndGap = NULL,
                        maxMiddleGap = NULL,
                        keepSpecies = NULL,
                        removeSpecies = NULL,
                        ClipFirst = FALSE,
                        ClipLast = FALSE){
  
  # load the rdata
  load(indata)
  
  # get the species list
  spp_list <- stacked_samps$species
  
  # do some checks
  if(any(!is.null(c(minObs, maxStartGap, maxEndGap, maxMiddleGap)))){
    # check there is metadata if it is needed by the filters
    if(is.null(metadata)){
    stop("Metadata is required for this set of filters but not supplied")}
    # check no species are missing
    if(length(setdiff(spp_list, metadata$Species)) > 0){
      stop("Not all species have metadata available. Please ensure all species are included in the metadata and that all names match")}
  }
  
  if(!is.null(keepSpecies)){
    # check all requested species are in indata
    if(length(setdiff(keepSpecies, spp_list)) > 0){
      warning("Not all keepSpecies are contained within the indata")}
    if(!is.null(removeSpecies)){
      # check that if both keep and remove species are given that they don't overlap
      if(length(setdiff(keepSpecies, removeSpecies) < length(keepSpecies))){
        stop("Some species are listed under both keepSpecies and removeSpecies. Please ensure these vectors do not overlap")}
    }
  }
  
  # retain only keepSpecies
  if(!is.null(keepSpecies)){
    stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% keepSpecies))
  }
  
  # remove any removeSpecies
  if(!is.null(removeSpecies)){
    stacked_samps <- droplevels(subset(stacked_samps, !(stacked_samps$species %in% removeSpecies)))
  }

  if(is.null(region)){
    # filter by minObs
    if(!is.null(minObs)){
      goodspecies <- droplevels(subset(metadata, metadata$n_obs >= minObs))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    # filter by gap start
    if(!is.null(maxStartGap)){
      goodspecies <- droplevels(subset(metadata, metadata$gap_start <= maxStartGap))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    # filter by gap end
    if(!is.null(maxEndGap)){
      goodspecies <- droplevels(subset(metadata, metadata$gap_end <= maxEndGap))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    # filter by gap middle
    if(!is.null(maxMiddleGap)){
      goodspecies <- droplevels(subset(metadata, metadata$gap_middle <= maxMiddleGap))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    
  }else{
    # if regional
    if(!is.null(minObs)){
      goodspecies <- droplevels(subset(metadata, metadata[,paste0("n_obs_r_",region)] >= minObs))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    # filter by gap start
    if(!is.null(maxStartGap)){
      goodspecies <- droplevels(subset(metadata, metadata[,paste0("gap_start_r_",region)] <= maxStartGap))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    # filter by gap end
    if(!is.null(maxEndGap)){
      goodspecies <- droplevels(subset(metadata, metadata[,paste0("gap_end_r_",region)] <= maxEndGap))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
    # filter by gap middle
    if(!is.null(maxMiddleGap)){
      goodspecies <- droplevels(subset(metadata, metadata[,paste0("gap_middle_r_",region)] <= maxMiddleGap))
      stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% goodspecies$Species))
    }
  }
  
  # first year clip
  if(ClipFirst == TRUE){
    if(is.null(region)){
  # loop through species list #
      for (i in spp_list){
      min_year_data <- metadata[metadata$Species == i, "min_year_data"]
      min_year_model <- metadata[metadata$Species == i, "min_year_model"]
    
      # replace the early years where there were no records of the focal species with NA
        if(min_year_data > min_year_model) {
        stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% paste0("year_", min_year_model:(min_year_data-1))] <- NA
        } 
      }
    }else{
      for (i in spp_list){
        min_year_data <- metadata[metadata$Species == i, paste0("min_year_data_r_", region)]
        min_year_model <- metadata[metadata$Species == i, paste0("min_year_model_r_", region)]
      
        # replace the early years where there were no records of the focal species with NA
        if(min_year_data > min_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% paste0("year_", min_year_model:(min_year_data-1))] <- NA
        } 
      }
    }
  }
  
  # last year clip
  if(ClipLast == TRUE){
    if(is.null(region)){
      # loop through species list 
      for (i in spp_list){
        max_year_data <- metadata[metadata$Species == i, "max_year_data"]
        max_year_model <- metadata[metadata$Species == i, "max_year_model"]
        
        # replace the later years where there were no records of the foacl species with NA
        if(max_year_data < max_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% paste0("year_", (max_year_data +1):max_year_model)] <- NA
        }
      }
    }else{
      for (i in spp_list){
        max_year_data <- metadata[metadata$Species == i, paste0("max_year_data_r_", region)]
        max_year_model <- metadata[metadata$Species == i, paste0("max_year_model_r_", region)]
    
        # replace the later years where there were no records of the foacl species with NA
        if(max_year_data < max_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% paste0("year_", (max_year_data +1):max_year_model)] <- NA
        }
      }
    }
  }
  save(stacked_samps, file = paste0(output_path, group_name, "stacked_Filter", ".rdata")) 
}