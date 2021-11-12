#' PlotOcc - Produce time-series plots of annual occupancy estimates
#' 
#' @description This function plots species-level annual occupancy time-series. The
#' function plots all species within the input folder.  Currently had a hard coded name_
#' match file that I will needs to remove
#' 
#' @param indata The folder containing the species-level occupancy model outputs
#' @param output_path The location where the output plots will be saved.
#' @param REGION_IN_Q The region for which the time-series that will be plotted.
#'      UK based occupancy model examples include,
#'		  'psi.fs', 'psi.fs.r_GB', 'psi.fs.r_ENGLAND'.
#' @param y_axis_choice Two choices here, "variable" or "fixed". Fixed, fixes the 
#'      y axis limits between 0 and 1. Variable sets the y axis limits equal to the 
#'      maximum and minimum plotted values.
#' @param naming_file This is a UK/BRC specific additon to add a file for matching concept
#'      codes to species names. Two option here "yes" or "no". if "yes" a custom naming
#'      file (called "spmod.csv") should be added to the '../data/' location.
#' @param cluster_run This is a UK specific parameter. "Jasmin" is used for outputs
#'      from Jasmin occupancy model runs.  "Cirrus" is used for standard .rdata occupancy 
#'      model outputs, such as those from the occDetFunc function from the R package Sparta.     
#' @param jasmin_min_year If running on Jasmin outputs, the first year must be specified
#'
#' @return A .png file for each species containing time-series plots of annual occupancy.
#' @import ggplot2    
#' @export

PlotOcc <- function(indata = "../data/model_runs/", 
                    output_path = "../output/", 
                    REGION_IN_Q = "^psi.fs\\[", 
                    y_axis_choice = "variable",
                    naming_file = "no",
                    cluster_run = "jasmin",
                    jasmin_min_year = 1970){
  
  # what are we running, used for file names later #
  if(REGION_IN_Q == "^psi.fs\\["){
    region_run <- "full_psi_fs"
  } else {
    region_run <- REGION_IN_Q
  }
  
  # load a name matching file
  if(naming_file == "yes"){
    name_match <- read.csv("../data/spmod.csv", header = TRUE)
  }
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata) # species for which we have models
  
  if (cluster_run == "cirrus"){
    spp_data <- spp.list[grepl(".rdata", spp.list)]
  }
  
  if (cluster_run == "jasmin"){
    spp_data <- spp.list[grepl(".csv", spp.list)]
  }
    
  ## Add a table that identifies how many records each species had each year, for each region ##
  ## Add if statement to select which years to plot ##
  
  ## Add code to run the model for one species ##
  for (i in spp_data) {
    print(i)
    
    # save species name #
    if(cluster_run == "cirrus"){
      spp_name <- gsub(".rdata", "", i)
    }
    
    if(cluster_run == "jasmin"){
      spp_name <- gsub(".csv", "", i)
    }
    
    if(cluster_run == "jasmin"){
      spp_name <- substr(spp_name, 1, (regexpr("_it",spp_name)[1]-1))
    }
    
    if(naming_file == "yes"){
      spp_name <- as.character(name_match[name_match$NAME_USED == spp_name, "SPECIES_NAME"])
    }
    
    if (cluster_run == "cirrus"){
      out <- NULL
      load(paste(indata, i, sep = ""))
      temp_data <- data.frame(out$BUGSoutput$summary)
      new_data <- temp_data[grep(REGION_IN_Q, row.names(temp_data)),]
      
      # get rows we are interested in
      ### take psi.fs rows - these are the yearly proportion of occupied cells ###
      #new_data$year <- (Year = (out$min_year - 1) + as.numeric(gsub(REGION_IN_Q, "", gsub("\\[|\\]","", row.names(new_data)))))
      new_data$year <- out$min_year:out$max_year 
      
      # rename columns for ggplot
      names(new_data) <- gsub("X2.5.","quant_025", names(new_data))
      names(new_data) <- gsub("X97.5.","quant_975", names(new_data))
      
      # Add rhat T/F column
      new_data$rhat_threshold[new_data$Rhat < 1.1] <- 'Good (<1.1)'
      new_data$rhat_threshold[new_data$Rhat > 1.1] <- 'Bad (>1.1)'
    }
    
    if (cluster_run == "jasmin"){
      temp_data <- NULL
      temp_data <- read.csv(paste(indata, i, sep = ""), header = TRUE)
      new_data <- temp_data[grep(REGION_IN_Q, temp_data$X),]
      
      # get rows we are interested in
      ### take psi.fs rows - these are the yearly proportion of occupied cells ###
      new_data$year <- jasmin_min_year:(jasmin_min_year + (nrow(new_data)-1))
      
      # rename columns for ggplot
      names(new_data) <- gsub("X2.5.","quant_025", names(new_data))
      names(new_data) <- gsub("X97.5.","quant_975", names(new_data))
      
      # Add rhat T/F column
      names(new_data) <- gsub("PSRF", "Rhat", names(new_data))
      new_data$rhat_threshold[new_data$Rhat < 1.1] <- 'Good (<1.1)'
      new_data$rhat_threshold[new_data$Rhat > 1.1] <- 'Bad (>1.1)'
      
      # ensure mean is lower case #
      names(new_data) <- gsub("Mean", "mean", names(new_data))
    }
    
    ### plot the yearly predicted proportion of occupied sites ###
    # plot with error bars based on 95CI #
    
    # two plots: one with fixed y-axis between 0 and 1, the other is variable y axis and driven by the data
    
    # fixed y axis #
    if (y_axis_choice == "fixed"){
      ggplot(new_data, aes_string(x = "year", y = "mean")) + 
        theme_bw() +
        geom_ribbon(data = new_data,
                    aes_string(group = 1, ymin = "quant_025", ymax = "quant_975"),
                    alpha = 0.2) +
        geom_line(size = 1, col = "black") +
        geom_point(size = 4, aes(col = rhat_threshold, shape = recs)) +
        # scale_shape_manual(values=c("data" = 19, "no data" = 4)) + #use when we create the records per region table
        scale_color_manual(name = 'Rhat', values = c('Bad (>1.1)' = 'red','Good (<1.1)' = 'blue')) +
        ylab("Occupancy") +
        xlab("Year") +
        scale_y_continuous(limits = c(0, 1)) +
        ggtitle(paste(spp_name)) # , " - n = ",  no_recs[no_recs$spp == spp_name, "no_recs"], sep = "")) + # the latter half to add when we have records info per region
      #theme(plot.title = element_text(lineheight = .8, face = "bold"), legend.position = 'bottom')
      #theme(legend.position = "none")
      
      ggsave(paste(spp_name, "_", region_run, ".png", sep = ""), plot = last_plot(), path = output_path, width=7, height=5.5, units="in", dpi = 100)	
    }
    
    if (y_axis_choice == "variable"){
      # fixed y axis #
      ggplot(new_data, aes_string(x = "year", y = "mean")) + 
        theme_bw() +
        geom_ribbon(data = new_data,
                    aes_string(group = 1, ymin = "quant_025", ymax = "quant_975"),
                    alpha = 0.2) +
        geom_line(size = 1, col = "black") +
        geom_point(size = 4, aes(col = rhat_threshold)) +
        #scale_shape_manual(values=c(19, 4)) +
        scale_color_manual(name = 'Rhat', values = c('Bad (>1.1)' = 'red','Good (<1.1)' = 'blue')) +
        ylab("Occupancy") +
        xlab("Year") +
        scale_y_continuous(limits = c(min(new_data$quant_025), max(new_data$quant_975))) +
        ggtitle(paste(spp_name)) # , " - n = ",  no_recs[no_recs$spp == spp_name, "no_recs"], sep = "")) + # the latter half to add when we have records info per region
      #theme(plot.title = element_text(lineheight = .8, face = "bold"), legend.position = 'bottom')
      #theme(legend.position = "none")
      
      ggsave(paste(spp_name, "_", region_run, ".png", sep = ""), plot = last_plot(), path = output_path, width=7, height=5.5, units="in", dpi = 300)	
    }  
    
  }
  
}
