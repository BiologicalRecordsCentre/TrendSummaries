#' SampBind - Combine the multiple outputs from SampPost
#' 
#' @description This function combines sets of samples created by \code{SampPost()}.
#'              This function can cobine samples with different start and end years
#'              if max and min years are available in the input attributes.  
#'
#' @param indata The file path to a location containing .rdata files
#'      of the \code{SampPost()} outputs, one for each species or species group.
#' @param innames A vector of file names to be used as the input. If a vector is
#'        not supplied then all .rdata files in the specified folder will be used.
#' @param output_path The location where the output will be saved.
#' @param group_name The name of the species group we are running, used for
#'  	  naming output files.
#'
#' @param write Logical. If TRUE writes output to output_path. 
#'  	  
#' @return An .rdata file containing the inputted posterior samples combined into
#'         a single dataframe.
#' @export
#' @importFrom plyr rbind.fill


SampBind <- function(indata = "../data/model_runs/", innames = NULL,
                     output_path = "../data/bound_samples/",
                     group_name = "",
                     write){
  
  # get the input list
  inputs.list <- list.files(indata, pattern = "*.rdata")
  if(!is.null(innames)){
    inputs.list <- subset(inputs.list, inputs.list %in% innames)
  }
  
  # get the size of the time span by looping through inputs
  max_yrs<-c()
  min_yrs<-c()

  for (i in 1:length(inputs.list)){
    Fnme <- inputs.list[i]
    samp_post <- NULL
    load(paste0(indata, Fnme))
    cur_attr <- attributes(samp_post)
    max_yrs[i] <- cur_attr$max_year_model
    min_yrs[i] <- cur_attr$min_year_model
  }
  
  ystart <- min(min_yrs)
  yfinish <- max(max_yrs)
  span <- yfinish - ystart
  
  yearsneeded <- paste0("year_", ystart:yfinish)
  colsneeded <- c(yearsneeded, "iteration", "species")
  
  templatedf <- setNames(data.frame(matrix(ncol = length(colsneeded), nrow = 0)), colsneeded)
  
  for (j in inputs.list){
    load(paste0(indata, j))
    templatedf <- rbind.fill(templatedf,samp_post)
  }
  
  stacked_samps <- templatedf
  
  if (write == TRUE) {
    save(stacked_samps, file = paste0(output_path, group_name, "stacked", ".rdata"))
  }
  
  return(stacked_samps)
  
}