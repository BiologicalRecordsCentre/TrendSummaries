# add packages #
require(ggplot2)
require(reshape2)
require(boot)
geomean <- function(x){exp(mean(log(x)))}

quan_0.05 <- function(x) quantile(x, probs = 0.05, na.rm = TRUE)
quan_0.95 <- function(x) quantile(x, probs = 0.95, na.rm = TRUE)
quan_0.5 <- function(x) quantile(x, probs = 0.5, na.rm = TRUE)
cust_a_mean <-  function(x) mean(x, na.rm = T)

# input data: the sampled 1000 iterations of the annual occupancy posterior distributions stacked across species
# format should be as follows: dataframe - columns = years "X1980, X1981, Xn, spp, iteration
# drop bad years and bad species prior to running this function

PlotOcc <- function(samp_post = "W:/PYWELL_SHARED/Pywell Projects/BRC/Gary/Indicator 2018/D1c pollinators/data/pollinators_1980_2013_50_dropped_samp_post.rdata", 
                    output_path = "outputs/",
                    trend_choice = "arithmetic_raw_occ",
                    group_name = "pollinators",
                    save_iterations = "yes"){
  
  load(samp_post)
  
  number_of_spp <- length(unique(as.character(samp_post$spp))) # HOW MANY SPECIES GOING INTO THE INDICATOR? # 214 hoverflies # 139 bees
  
  # loop through iterations - later convert to array and apply across array, should be quicker. #
  composite_trend <- NULL
  for (j in 1:length(unique(samp_post$iter))){
    print(j)
    temp_table <- NULL
    temp_table <- samp_post[samp_post$iter == j,]
    t_table <- temp_table[,(1:(ncol(temp_table)-2))] # convert shape of the table
    
    # arithmean on the occ scale #
    logit_temp_table <- t_table
    logit_temp_table <- as.data.frame(logit(as.matrix(logit_temp_table)))
    
    # geomean on the occ scale #
    log_temp_table <- t_table
    log_temp_table <- log(log_temp_table)
    
    # geometric mean raw occupancy #
    if(trend_choice == "geometric_raw_occ"){
      composite_trend_temp <- apply(t_table, 2, geomean)
      composite_trend <- rbind(composite_trend, composite_trend_temp)
    }
    
    # arithmetic mean raw occupancy #
    if(trend_choice == "arithmetic_raw_occ"){
      composite_trend_temp <- apply(t_table, 2, mean)
      composite_trend <- rbind(composite_trend, composite_trend_temp)
    }
    
    # arithmetic log odds (logit) occupancy back converted to occupancy scale with inv.logit
    if(trend_choice == "arithmetic_logit_occ"){
      composite_trend_temp <- apply(logit_temp_table, 2, mean)
      composite_trend <- rbind(composite_trend, composite_trend_temp)
    }
      
    # geometric log odds (logit) occupancy back converted to odds scale with exp
    if(trend_choice == "geometric_logit_occ"){
      composite_trend_temp <- apply(logit_temp_table, 2, geomean)
      composite_trend <- rbind(composite_trend, composite_trend_temp)
    }
  }
  
  if(save_iterations == "yes"){
    write.csv(composite_trend, file = output_path, row.names = FALSE)
  }
    
  # save the summarised iterations #
  composite_trend_summary <- data.frame(
    year = as.numeric(gsub("X", "", colnames(composite_trend))),
    mean_occupancy = apply(composite_trend, 2, mean),
    median_occupancy = apply(composite_trend, 2, median),
    lower_5_perc_CI_occ = apply(composite_trend, 2, quan_0.05),
    upper_95_perc_CI_occ = apply(composite_trend, 2, quan_0.95)
  )
    
  
}